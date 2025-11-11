CREATE TYPE e_offset_generator_type AS ENUM(
  'none',
  'enumerated',
  'grid',
  'random',
  'spiral'
);

CREATE TABLE t_offset_generator (

  c_offset_generator_id   SERIAL                  PRIMARY KEY,
  c_observation_id        d_observation_id        NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_offset_generator_type e_offset_generator_type NOT NULL,

  c_grid_corner_a_p       d_angle_µas             NOT NULL DEFAULT 0,
  c_grid_corner_a_q       d_angle_µas             NOT NULL DEFAULT 0,
  c_grid_corner_b_p       d_angle_µas             NOT NULL DEFAULT 0,
  c_grid_corner_b_q       d_angle_µas             NOT NULL DEFAULT 0,

  c_size                  d_angle_µas             NOT NULL DEFAULT 0,
  c_center_offset_p       d_angle_µas             NOT NULL DEFAULT 0,
  c_center_offset_q       d_angle_µas             NOT NULL DEFAULT 0

);

CREATE TABLE t_enumerated_offset (

  c_offset_generator_id int           NOT NULL REFERENCES t_offset_generator(c_offset_generator_id) ON DELETE CASCADE,
  c_index               int           NOT NULL,

  PRIMARY KEY (c_offset_generator_id, c_index),

  c_offset_p            d_angle_µas   NOT NULL DEFAULT 0,
  c_offset_q            d_angle_µas   NOT NULL DEFAULT 0,
  c_guide_state         e_guide_state NOT NULL DEFAULT 'enabled'

);

ALTER TABLE t_gmos_north_imaging_filter
  ADD COLUMN c_seed bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint;

ALTER TABLE t_gmos_north_imaging
  ADD COLUMN c_object_offset_generator_id int    REFERENCES t_offset_generator(c_offset_generator_id),
  ADD COLUMN c_sky_seed                   bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint,
  ADD COLUMN c_sky_offset_generator_id    int    REFERENCES t_offset_generator(c_offset_generator_id);

ALTER TABLE t_gmos_south_imaging_filter
  ADD COLUMN c_seed bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint;

ALTER TABLE t_gmos_south_imaging
  ADD COLUMN c_object_offset_generator_id int    REFERENCES t_offset_generator(c_offset_generator_id),
  ADD COLUMN c_sky_seed                   bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint,
  ADD COLUMN c_sky_offset_generator_id    int    REFERENCES t_offset_generator(c_offset_generator_id);
