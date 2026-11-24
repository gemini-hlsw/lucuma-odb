CREATE TYPE e_scorpio_channel_selection AS ENUM(
  'vis_ir',
  'vis_only',
  'ir_only'
);

CREATE TYPE e_scorpio_vis_readout_mode AS ENUM(
  'low_noise',
  'fast'
);

CREATE TYPE e_scorpio_disperser AS ENUM(
  'out',
  'vph'
);

CREATE TABLE t_scorpio_static (
  c_static_id         bigserial                   PRIMARY KEY,
  c_observation_id    d_observation_id            NOT NULL REFERENCES t_observation (c_observation_id),
  c_instrument        d_tag                       NOT NULL DEFAULT ('Scorpio'),

  FOREIGN KEY (c_observation_id, c_instrument)
  REFERENCES t_observation (c_observation_id, c_instrument),

  CHECK (c_instrument = 'Scorpio'),

  CONSTRAINT t_scorpio_static_obs_unique UNIQUE (c_observation_id),

  c_channel_selection e_scorpio_channel_selection NOT NULL
);

-- Only the visible channels and the long slit are stored.  A channel that is
-- off has all three of its columns null.
CREATE TABLE t_scorpio_dynamic (
  c_step_id          d_step_id                  PRIMARY KEY,
  c_instrument       d_tag                      NOT NULL DEFAULT ('Scorpio'),

  FOREIGN KEY (c_step_id, c_instrument)
  REFERENCES t_step (c_step_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,

  CHECK (c_instrument = 'Scorpio'),

  c_g_exposure_time  interval,
  c_g_exposure_count int4                       CHECK (c_g_exposure_count > 0),
  c_g_readout_mode   e_scorpio_vis_readout_mode,
  CONSTRAINT scorpio_dynamic_g_check
    CHECK (num_nulls(c_g_exposure_time, c_g_exposure_count, c_g_readout_mode) IN (0, 3)),

  c_r_exposure_time  interval,
  c_r_exposure_count int4                       CHECK (c_r_exposure_count > 0),
  c_r_readout_mode   e_scorpio_vis_readout_mode,
  CONSTRAINT scorpio_dynamic_r_check
    CHECK (num_nulls(c_r_exposure_time, c_r_exposure_count, c_r_readout_mode) IN (0, 3)),

  c_i_exposure_time  interval,
  c_i_exposure_count int4                       CHECK (c_i_exposure_count > 0),
  c_i_readout_mode   e_scorpio_vis_readout_mode,
  CONSTRAINT scorpio_dynamic_i_check
    CHECK (num_nulls(c_i_exposure_time, c_i_exposure_count, c_i_readout_mode) IN (0, 3)),

  c_z_exposure_time  interval,
  c_z_exposure_count int4                       CHECK (c_z_exposure_count > 0),
  c_z_readout_mode   e_scorpio_vis_readout_mode,
  CONSTRAINT scorpio_dynamic_z_check
    CHECK (num_nulls(c_z_exposure_time, c_z_exposure_count, c_z_readout_mode) IN (0, 3)),

  c_fpu              d_tag                      NOT NULL REFERENCES t_scorpio_fpu(c_tag),
  c_disperser        e_scorpio_disperser        NOT NULL
);

-- A channel sub-object is null when its synthetic id is.
CREATE VIEW v_scorpio_dynamic AS
  SELECT t.*,
  CASE WHEN t.c_g_exposure_time IS NOT NULL THEN t.c_step_id END AS c_g_id,
  CASE WHEN t.c_r_exposure_time IS NOT NULL THEN t.c_step_id END AS c_r_id,
  CASE WHEN t.c_i_exposure_time IS NOT NULL THEN t.c_step_id END AS c_i_id,
  CASE WHEN t.c_z_exposure_time IS NOT NULL THEN t.c_step_id END AS c_z_id
FROM
  t_scorpio_dynamic t;
