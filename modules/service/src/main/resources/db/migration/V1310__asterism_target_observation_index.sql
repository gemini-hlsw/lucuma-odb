-- The primary key leads with c_program_id, so a lookup by observation alone
-- (how the target environment is joined) scans the whole table.
CREATE INDEX i_asterism_target_observation
  ON t_asterism_target (c_observation_id);
