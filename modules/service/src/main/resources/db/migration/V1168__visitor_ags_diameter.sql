-- Rename c_science_fov to c_ags_diameter
DROP VIEW v_visitor;

ALTER TABLE t_visitor
  RENAME COLUMN c_science_fov TO c_ags_diameter;

UPDATE t_visitor
  SET c_ags_diameter = 60000000
  WHERE c_observing_mode_type = 'maroon_x';

CREATE VIEW v_visitor AS
  SELECT
    v.*,
    CASE WHEN v.c_total_request_time IS NOT NULL THEN v.c_observation_id END
      AS c_total_request_time_id
  FROM t_visitor v;
