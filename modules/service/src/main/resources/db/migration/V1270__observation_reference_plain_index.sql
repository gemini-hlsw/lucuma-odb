-- Drops the redundant UNIQUE on t_observation.c_observation_reference.
--
-- WHAT HAPPENED
--
-- c_observation_reference is GENERATED and UNIQUE.  Postgres recomputes a
-- generated column on every update and counts it as touched even when the value
-- does not change; because it sits in a unique index, EVERY update of an
-- observation -- c_title included -- took FOR UPDATE on the row.
--
-- WHAT CHANGES
--
-- Drop the UNIQUE, keep i_observation_reference (V0847, non-unique) -- which is
-- what the service's lookups by reference actually use.
--
-- RESULT
--
-- Observation updates take FOR NO KEY UPDATE and no longer block child-row
-- inserts; lookups are unchanged.  Uniqueness still holds by construction:
-- program references are unique, and c_observation_index is unique per program,
-- auto-assigned and frozen after insert.

ALTER TABLE t_observation
  DROP CONSTRAINT t_observation_c_observation_reference_key;

COMMENT ON COLUMN t_observation.c_observation_reference IS
  'Generated from the program reference and the observation index. Unique by '
  'construction rather than by constraint (see V1270): a generated column in a '
  'unique index forces every update of the row to take FOR UPDATE.';