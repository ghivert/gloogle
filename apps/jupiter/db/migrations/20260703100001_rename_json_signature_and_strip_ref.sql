-- migrate:up

-- 1. Rename for consistency with the other `signature_*` columns.
alter table package_type_fun_signature
  rename column json_signature to signature_json;

-- 2. Strip every `ref` key from the stored JSON. The cross-reference is no longer
--    frozen at ingestion (it resolves a semver *range* against an evolving version
--    set, so it must be resolved at query time). `ref` is nested inside every
--    `named` node, so a recursive walk is needed. plpgsql resolves the self-call
--    at runtime, so recursion is safe.
create function jsonb_strip_key(data jsonb, target text) returns jsonb
  language plpgsql immutable as $$
  declare
    result jsonb;
    k text;
    v jsonb;
  begin
    case jsonb_typeof(data)
      when 'object' then
        result := '{}'::jsonb;
        for k, v in select * from jsonb_each(data - target) loop
          result := result || jsonb_build_object(k, jsonb_strip_key(v, target));
        end loop;
        return result;
      when 'array' then
        result := '[]'::jsonb;
        for v in select * from jsonb_array_elements(data) loop
          result := result || jsonb_build_array(jsonb_strip_key(v, target));
        end loop;
        return result;
      else
        return data;
    end case;
  end;
  $$;

update package_type_fun_signature
  set signature_json = jsonb_strip_key(signature_json, 'ref')
  where signature_json is not null;

drop function jsonb_strip_key(jsonb, text);

-- migrate:down

-- The stripped `ref` values cannot be restored (re-derive them by re-ingesting);
-- only the rename is reversible.
alter table package_type_fun_signature
  rename column signature_json to json_signature;
