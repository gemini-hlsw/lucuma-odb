-- Descriptive name and total requested time for a visitor observation.
-- Both are required for the alen visitor_north and visitor_south modes.
ALTER TABLE t_visitor
  ADD COLUMN c_name text NULL,
  ADD COLUMN c_total_request_time interval NULL,
  ADD CONSTRAINT visitor_requires_name_and_time
    CHECK (
      c_observing_mode_type NOT IN ('visitor_north', 'visitor_south')
      OR (c_name IS NOT NULL AND c_total_request_time IS NOT NULL)
    );

CREATE VIEW v_visitor AS
  SELECT
    v.*,
    CASE WHEN v.c_total_request_time IS NOT NULL THEN v.c_observation_id END
      AS c_total_request_time_id
  FROM t_visitor v;
