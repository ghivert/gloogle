#!/usr/bin/env python3
"""Mirror the production object-storage bucket (Scaleway) into the local stack.

Two hops, so the copy on disk doubles as a backup and a reseed never hits
Scaleway twice:

    Scaleway s3://<bucket>  →  infra/local/dumps/bucket-<bucket>/  →  RustFS s3://gloogle

Objects are compared by size (and skipped when already present), so the script
is idempotent and resumable. Credentials come from the environment, as jupiter
reads them (`mise env` in apps/jupiter exports them):

    BUCKET_URI             virtual-hosted host, e.g. hex-mirror.s3.fr-par.scw.cloud
    SCALEWAY_ACCESS_KEY / SCALEWAY_SECRET_KEY

Local S3 defaults match infra/local/compose.yml and can be overridden with
LOCAL_S3_ENDPOINT, LOCAL_S3_ACCESS_KEY, LOCAL_S3_SECRET_KEY, LOCAL_BUCKET.

    mirror_bucket.py measure      count + size of the production bucket
    mirror_bucket.py download     Scaleway → disk
    mirror_bucket.py upload       disk → RustFS (creates the bucket)
    mirror_bucket.py mirror       download then upload
"""
import os
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import boto3
from botocore.config import Config

ROOT = Path(__file__).resolve().parents[3]
DUMPS = ROOT / "infra" / "local" / "dumps"
WORKERS = int(os.environ.get("MIRROR_WORKERS", "16"))


def env(name, default=None):
    value = os.environ.get(name, default)
    if value is None:
        sys.exit(f"missing env var: {name}")
    return value


def source():
    host = env("BUCKET_URI")
    bucket, _, endpoint_host = host.partition(".")
    client = boto3.client(
        "s3",
        endpoint_url=f"https://{endpoint_host}",
        region_name="fr-par",
        aws_access_key_id=env("SCALEWAY_ACCESS_KEY"),
        aws_secret_access_key=env("SCALEWAY_SECRET_KEY"),
        config=Config(max_pool_connections=WORKERS * 2, retries={"max_attempts": 8}),
    )
    return client, bucket


def destination():
    client = boto3.client(
        "s3",
        endpoint_url=env("LOCAL_S3_ENDPOINT", "http://localhost:29000"),
        region_name="fr-par",
        aws_access_key_id=env("LOCAL_S3_ACCESS_KEY", "gloogle"),
        aws_secret_access_key=env("LOCAL_S3_SECRET_KEY", "gloogle-local"),
        config=Config(
            s3={"addressing_style": "path"},
            max_pool_connections=WORKERS * 2,
            retries={"max_attempts": 8},
        ),
    )
    return client, env("LOCAL_BUCKET", "gloogle")


def list_objects(client, bucket):
    paginator = client.get_paginator("list_objects_v2")
    for page in paginator.paginate(Bucket=bucket):
        for obj in page.get("Contents", []):
            yield obj["Key"], obj["Size"]


def human(n):
    for unit in ("B", "KiB", "MiB", "GiB", "TiB"):
        if n < 1024:
            return f"{n:.1f} {unit}"
        n /= 1024
    return f"{n:.1f} PiB"


def measure():
    client, bucket = source()
    count = total = 0
    for _, size in list_objects(client, bucket):
        count += 1
        total += size
    print(f"s3://{bucket}: {count} objects, {human(total)} ({total} bytes)")


def run_parallel(label, jobs, worker):
    done = skipped = 0
    failures = []
    with ThreadPoolExecutor(max_workers=WORKERS) as pool:
        futures = {pool.submit(worker, *job): job for job in jobs}
        for future in as_completed(futures):
            key = futures[future][0]
            try:
                if future.result():
                    done += 1
                else:
                    skipped += 1
            except Exception as exc:  # noqa: BLE001 — report, keep going
                failures.append((key, repr(exc)))
            if (done + skipped + len(failures)) % 500 == 0:
                print(f"  {label}: {done} copied, {skipped} skipped, {len(failures)} failed", flush=True)
    print(f"{label}: {done} copied, {skipped} skipped, {len(failures)} failed")
    for key, err in failures[:20]:
        print(f"  FAILED {key}: {err}")
    return not failures


def download():
    client, bucket = source()
    mirror = DUMPS / f"bucket-{bucket}"
    mirror.mkdir(parents=True, exist_ok=True)
    print(f"→ Scaleway s3://{bucket} → {mirror}")

    def fetch(key, size):
        target = mirror / key
        if target.exists() and target.stat().st_size == size:
            return False
        target.parent.mkdir(parents=True, exist_ok=True)
        tmp = target.with_suffix(target.suffix + ".part")
        client.download_file(bucket, key, str(tmp))
        tmp.replace(target)
        return True

    return run_parallel("download", list(list_objects(client, bucket)), fetch)


def upload():
    _, src_bucket = source()
    client, bucket = destination()
    mirror = DUMPS / f"bucket-{src_bucket}"
    if not mirror.is_dir():
        sys.exit(f"no local mirror at {mirror}; run download first")
    try:
        client.head_bucket(Bucket=bucket)
    except Exception:  # noqa: BLE001
        client.create_bucket(Bucket=bucket)
        print(f"created bucket {bucket}")
    present = dict(list_objects(client, bucket))
    print(f"→ {mirror} → local s3://{bucket} ({len(present)} objects already there)")
    files = [
        (str(p.relative_to(mirror)), p.stat().st_size)
        for p in mirror.rglob("*")
        if p.is_file() and not p.name.endswith(".part")
    ]

    def push(key, size):
        if present.get(key) == size:
            return False
        client.upload_file(str(mirror / key), bucket, key)
        return True

    return run_parallel("upload", files, push)


COMMANDS = {
    "measure": lambda: measure() or True,
    "download": download,
    "upload": upload,
    "mirror": lambda: download() and upload(),
}

if __name__ == "__main__":
    command = sys.argv[1] if len(sys.argv) > 1 else "mirror"
    if command not in COMMANDS:
        sys.exit(f"usage: {sys.argv[0]} [{'|'.join(COMMANDS)}]")
    sys.exit(0 if COMMANDS[command]() else 1)
