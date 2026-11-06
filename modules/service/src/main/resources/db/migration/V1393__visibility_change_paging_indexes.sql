-- The scheduler reads visibility changes in pages ordered by invalidation time
-- and then id.  Indexing both lets a page start exactly at its cursor, even
-- inside a run of rows that share one invalidation time, so that a poll for
-- recent changes reads only those rows.  The new indices serve every lookup
-- the time-only ones did.
DROP INDEX observation_visibility_last_invalidation_index;
CREATE INDEX observation_visibility_last_invalidation_index
  ON t_observation_visibility (c_last_visibility_invalidation, c_observation_id);

DROP INDEX target_visibility_last_invalidation_index;
CREATE INDEX target_visibility_last_invalidation_index
  ON t_target_visibility (c_last_visibility_invalidation, c_target_id);

-- The primary key leads with c_program_id, so a lookup by target alone (how
-- visibility changes check that a target is in a present observation's
-- asterism) scans the whole index.
CREATE INDEX i_asterism_target_target
  ON t_asterism_target (c_target_id);
