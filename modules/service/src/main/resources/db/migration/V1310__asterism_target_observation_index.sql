-- t_asterism_target's primary key leads with c_program_id, so a lookup by
-- observation alone -- which is how the target environment is joined, e.g. for
-- firstScienceTarget -- cannot use it and scans the whole table once per
-- observation.  Seen while tracing a proposal-summary render: 73 sequential
-- scans of 6,732 rows, 3,139 buffers, for 73 single-row lookups.
CREATE INDEX i_asterism_target_observation
  ON t_asterism_target (c_observation_id);
