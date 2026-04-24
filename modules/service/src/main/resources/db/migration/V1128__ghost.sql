CREATE TABLE t_smart_ghost (
  c_instrument d_tag NOT NULL CHECK (c_instrument = 'Ghost'),
  c_gcal_id    int4  NOT NULL,
  PRIMARY KEY (c_instrument, c_gcal_id),
  FOREIGN KEY (c_instrument, c_gcal_id) REFERENCES t_gcal(c_instrument, c_gcal_id),

  -- Matching key
  c_resolution_mode d_tag NOT NULL REFERENCES t_ghost_resolution_mode(c_tag),
  c_red_binning     d_tag NOT NULL REFERENCES t_ghost_binning(c_tag),
  c_blue_binning    d_tag NOT NULL REFERENCES t_ghost_binning(c_tag),

  c_step_order int8  NOT NULL DEFAULT 0 CHECK (c_step_order > 0),

  -- Instrument value
  c_red_exposure_time   interval NOT NULL CHECK (c_red_exposure_time >= interval '0 seconds'),
  c_red_exposure_count  integer  NOT NULL CHECK (c_red_exposure_count > 0),
  c_blue_exposure_time  interval NOT NULL CHECK (c_blue_exposure_time >= interval '0 seconds'),
  c_blue_exposure_count integer  NOT NULL CHECK (c_blue_exposure_count > 0),
  c_slit_viewing_camera_exposure_time interval NOT NULL CHECK (c_slit_viewing_camera_exposure_time >= interval '0 seconds')
);