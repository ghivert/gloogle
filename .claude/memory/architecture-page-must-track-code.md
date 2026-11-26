---
name: architecture-page-must-track-code
description: Any change that can alter the architecture must update architecture.html and spell out what changed — full rule lives in CLAUDE.md
metadata:
  type: feedback
---

Any code change that can alter the architecture **must** update `architecture.html` in the same
change, and **must spell out what changed in the architecture**.

**Authority: `CLAUDE.md` at the repo root.** It carries the full rule — the trigger paths, the
judgement clause on top of them, what "detail what changed" means, and the non-negotiables when
editing the page (both FR/EN languages, the status tags, self-containment, the leak check).
This file is only a pointer: **do not duplicate the rule here**, it would drift.

**Why:** `architecture.html` is read to answer "how does this work". A diagram that lies is worse
than no diagram: it is trusted, and it is wrong. The page was built by reading the source tree, so
it goes stale the moment the code moves without it. This is not hypothetical — the page shipped
stale on day one, because commit `b63a44c` moved `data/ir` into `galaxy` while the page was being
written.

**How to apply:** mechanical trigger (migrations, `schema.sql`, `data/ir*`, `galaxy/src/data/**`,
`jupiter.gleam`, `router.gleam`, `workers.gleam`, `queries.gleam`, `handlers.gleam`,
`api/signatures.gleam`, `tasks/**`) **plus** case-by-case judgement when a change reshapes the
system without touching those paths. Read `CLAUDE.md` before editing the page.

See [[multilang-canonical-signature-ir]] for the trajectory of the IR refactor the page documents,
and [[user-prefers-french-explanations]] for which language goes where.
