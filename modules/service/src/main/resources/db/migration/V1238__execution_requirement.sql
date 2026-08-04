-- Replaces the two-valued c_is_splittable boolean with a three-valued enum that
-- also captures uninterruptibility.
--
--   c_is_splittable = true   ->  unconstrained
--   c_is_splittable = false  ->  contiguous
--
-- 'uninterruptible' (neither splittable nor interruptible) was not expressible
-- before, so no existing row maps to it.  c_execution_requirement is now the
-- single source of truth; the deprecated c_is_splittable is recomputed in the
-- views as (c_execution_requirement = 'unconstrained').
--
-- Part 1 of 2.  Dropping the old c_is_splittable column and recreating the views
-- that reference it happens in V1236, NOT here: the backfill UPDATE below queues
-- (deferred) trigger events on t_observation, and PostgreSQL refuses to ALTER a
-- table that has pending trigger events within the same transaction.  Flyway runs
-- each migration in its own transaction, so V1236 gets a clean one after this
-- one's trigger events have fired at commit.

CREATE TYPE e_execution_requirement AS ENUM(
  'unconstrained',
  'contiguous',
  'uninterruptible'
);

ALTER TABLE t_observation
  ADD COLUMN c_execution_requirement e_execution_requirement NOT NULL DEFAULT 'unconstrained';

-- Every row starts at the 'unconstrained' default; only the non-splittable ones
-- differ, so touch just those.
UPDATE t_observation
   SET c_execution_requirement = 'contiguous'::e_execution_requirement
 WHERE NOT c_is_splittable;