-- schema support for the target-based WHERE filters.

-- B-tree indexes on configuration-request reference coordinates, to support
-- fast cone (angular-distance) searches via bounding-box prefiltering on the
-- int8 microarcsecond columns.
CREATE INDEX IF NOT EXISTS ix_configuration_request_reference_ra
  ON t_configuration_request (c_reference_ra);

CREATE INDEX IF NOT EXISTS ix_configuration_request_reference_dec
  ON t_configuration_request (c_reference_dec);

-- Computed `c_is_active` on v_program, true when the current UTC date falls
-- within the program's [c_active_start, c_active_end] window.
-- Evaluated in SQL so the filter pushes down and uses transaction-start time.
CREATE OR REPLACE VIEW v_program AS
  SELECT
    p.*,
    COALESCE(rc.c_resource_count, 0) AS c_resource_count,
    ((now() AT TIME ZONE 'UTC')::date BETWEEN p.c_active_start AND p.c_active_end) AS c_is_active
  FROM t_program p
  LEFT JOIN t_program_resource_count rc ON rc.c_program_id = p.c_program_id;
