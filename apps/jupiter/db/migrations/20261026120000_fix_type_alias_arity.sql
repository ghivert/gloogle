-- migrate:up
-- `ir.fingerprint` used to store `arity = 0` for every `TypeAlias`, while phase 2
-- (`unify_declaration`) matches a query against the *aliased* type. An alias of a
-- function type (`type Callback = fn(Int) -> Bool`) was therefore dropped by the
-- phase-1 filter `arity = $1` although it would have verified. The fingerprint now
-- derives the alias' arity from the aliased type, like `Constant`; this backfills
-- the rows written before the fix, straight from the stored IR.
update package_type_fun_signature
  set arity = case
    when signature_ir -> 'aliased' ->> 'kind' = 'fn'
      then jsonb_array_length(signature_ir -> 'aliased' -> 'params')
    else 0
  end
  where signature_ir ->> 'kind' = 'type-alias';

-- migrate:down
-- Restores the pre-fix value: every alias back at arity 0.
update package_type_fun_signature
  set arity = 0
  where signature_ir ->> 'kind' = 'type-alias';
