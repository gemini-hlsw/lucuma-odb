-- SC-9240: B-tree indexes on configuration-request reference coordinates,
-- to support fast cone (angular-distance) searches via bounding-box
-- prefiltering on the int8 microarcsecond columns. Pure-SQL trig; no PostGIS.
CREATE INDEX IF NOT EXISTS ix_configuration_request_reference_ra
  ON t_configuration_request (c_reference_ra);

CREATE INDEX IF NOT EXISTS ix_configuration_request_reference_dec
  ON t_configuration_request (c_reference_dec);
