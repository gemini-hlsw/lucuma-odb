CREATE TYPE e_offset_generator_type AS ENUM(
  'none',
  'enumerated',
  'grid',
  'random',
  'spiral'
);

CREATE TYPE e_offset_generator_role AS ENUM(
  'object',
  'sky'
);

CREATE TABLE t_offset_generator (

  c_observation_id   d_observation_id        NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_role             e_offset_generator_role NOT NULL,
  PRIMARY KEY (c_observation_id, c_role),

  c_type             e_offset_generator_type NOT NULL,

  c_grid_corner_a_p  d_angle_µas             NOT NULL DEFAULT 0,
  c_grid_corner_a_q  d_angle_µas             NOT NULL DEFAULT 0,
  c_grid_corner_b_p  d_angle_µas             NOT NULL DEFAULT 0,
  c_grid_corner_b_q  d_angle_µas             NOT NULL DEFAULT 0,

  c_size             d_angle_µas             NOT NULL DEFAULT 0,
  c_center_offset_p  d_angle_µas             NOT NULL DEFAULT 0,
  c_center_offset_q  d_angle_µas             NOT NULL DEFAULT 0

);

-- We'd like to have t_offset_generator reference the imaging modes so that
-- deletion could cascade.  Unfortunately though there are multiple imaging
-- modes.  Instead, we'll delete generators using a deletion trigger.
CREATE FUNCTION offset_generation_cleanup()
RETURNS TRIGGER AS $$
BEGIN
  DELETE FROM t_offset_generator
  WHERE c_observation_id = OLD.c_observation_id;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER offset_generation_cleanup_trigger
AFTER DELETE ON t_gmos_north_imaging
FOR EACH ROW
EXECUTE FUNCTION offset_generation_cleanup();

CREATE TRIGGER offset_generation_cleanup_trigger
AFTER DELETE ON t_gmos_south_imaging
FOR EACH ROW
EXECUTE FUNCTION offset_generation_cleanup();

-- We need a view with synthetic ids for grackle.
CREATE VIEW v_offset_generator AS
  SELECT o.*,
  CASE WHEN o.c_role = 'object'     THEN o.c_observation_id END AS c_object_observation_id,
  CASE WHEN o.c_role = 'sky'        THEN o.c_observation_id END AS c_sky_observation_id,
  CASE WHEN o.c_type = 'enumerated' THEN o.c_observation_id END AS c_enumerated_observation_id,
  CASE WHEN o.c_type = 'enumerated' THEN o.c_role           END AS c_enumerated_role,
  CASE WHEN o.c_type = 'grid'       THEN o.c_observation_id END AS c_grid_observation_id,
  CASE WHEN o.c_type = 'grid'       THEN o.c_role           END AS c_grid_role,
  CASE WHEN o.c_type = 'random'     THEN o.c_observation_id END AS c_random_observation_id,
  CASE WHEN o.c_type = 'random'     THEN o.c_role           END AS c_random_role,
  CASE WHEN o.c_type = 'spiral'     THEN o.c_observation_id END AS c_spiral_observation_id,
  CASE WHEN o.c_type = 'spiral'     THEN o.c_role           END AS c_spiral_role
  FROM t_offset_generator o;


CREATE TABLE t_enumerated_offset (

  c_observation_id   d_observation_id        NOT NULL,
  c_role             e_offset_generator_role NOT NULL,
  c_index            int                     NOT NULL,

  PRIMARY KEY (c_observation_id, c_role, c_index),
  FOREIGN KEY (c_observation_id, c_role) REFERENCES t_offset_generator (c_observation_id, c_role) ON DELETE CASCADE,

  c_offset_p         d_angle_µas   NOT NULL DEFAULT 0,
  c_offset_q         d_angle_µas   NOT NULL DEFAULT 0,
  c_guide_state      e_guide_state NOT NULL DEFAULT 'enabled'

);

CREATE VIEW v_enumerated_offset AS
  SELECT e.*,
  CASE WHEN e.c_role = 'object' THEN e.c_observation_id END AS c_object_observation_id,
  CASE WHEN e.c_role = 'sky'    THEN e.c_observation_id END AS c_sky_observation_id
  FROM t_enumerated_offset e;

ALTER TABLE t_gmos_north_imaging_filter
  ADD COLUMN c_seed      bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint;

ALTER TABLE t_gmos_north_imaging
  ADD COLUMN c_sky_seed  bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint;

ALTER TABLE t_gmos_south_imaging_filter
  ADD COLUMN c_seed      bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint;

ALTER TABLE t_gmos_south_imaging
  ADD COLUMN c_sky_seed  bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint;

DROP VIEW v_gmos_north_imaging;
CREATE VIEW v_gmos_north_imaging AS
  SELECT
    i.*,
    f.c_filters
  FROM
    t_gmos_north_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_gmos_north_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);

DROP VIEW v_gmos_south_imaging;
CREATE VIEW v_gmos_south_imaging AS
  SELECT
    i.*,
    f.c_filters
  FROM
    t_gmos_south_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_gmos_south_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);