CREATE TYPE e_ghost_fiber_agitator AS ENUM(
  'disabled',
  'enabled'
);

CREATE TABLE t_ghost_dynamic (
  c_step_id d_step_id PRIMARY KEY,
  c_instrument d_tag NOT NULL DEFAULT ('Ghost'),

  FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step (c_step_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,
  CHECK (c_instrument = 'Ghost'),

  c_blue_exposure_time  interval NOT NULL,
  c_blue_exposure_count int4     NOT NULL,
  c_blue_binning        d_tag    NOT NULL REFERENCES t_ghost_binning(c_tag),
  c_blue_read_mode      d_tag    NOT NULL REFERENCES t_ghost_read_mode(c_tag),

  c_red_exposure_time   interval NOT NULL,
  c_red_exposure_count  int4     NOT NULL,
  c_red_binning         d_tag    NOT NULL REFERENCES t_ghost_binning(c_tag),
  c_red_read_mode       d_tag    NOT NULL REFERENCES t_ghost_read_mode(c_tag),

  c_ifu1_fiber_agitator e_ghost_fiber_agitator NOT NULL DEFAULT 'disabled',
  c_ifu2_fiber_agitator e_ghost_fiber_agitator NOT NULL DEFAULT 'disabled'

);

CREATE TABLE t_ghost_static (

   c_static_id bigserial  PRIMARY KEY,
   c_visit_id  d_visit_id NULL REFERENCES t_visit (c_visit_id),
   UNIQUE NULLS NOT DISTINCT (c_observation_id, c_visit_id),

  c_observation_id d_observation_id NOT NULL,
  c_instrument     d_tag            NOT NULL DEFAULT ('Ghost'),

  -- We link back to the observation table thus.
  FOREIGN KEY (c_observation_id, c_instrument) REFERENCES t_observation (c_observation_id, c_instrument),
  CHECK (c_instrument = 'Ghost'),

  c_resolution_mode d_tag NOT NULL REFERENCES t_ghost_resolution_mode(c_tag)
);