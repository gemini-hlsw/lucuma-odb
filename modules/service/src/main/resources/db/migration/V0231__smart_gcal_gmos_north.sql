-- Smart gcal table for GMOS North.  A search is performed using items from the
-- search key columns and the corresponding gcal config in t_gcal is retrieved
-- for the matching row(s).
CREATE TABLE t_smart_gmos_north (

  c_instrument       d_tag NOT NULL,
  c_gcal_id          int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_gcal_id),

  FOREIGN KEY (c_instrument, c_gcal_id) REFERENCES t_gcal(c_instrument, c_gcal_id),
  CONSTRAINT check_is_gmos_north CHECK (c_instrument = 'GmosNorth'),

  -- When there are multiple Gcal configuration values matching the same key,
  -- the step order is used to order them.  If two or more have the same step
  -- order, they may be executed in any order.
  c_step_order       int8                  NOT NULL DEFAULT 0,
  CONSTRAINT check_positive_step_order CHECK (c_step_order > 0),

  -- GMOS North Search Key
  c_disperser        d_tag                          REFERENCES t_gmos_north_disperser(c_tag),
  c_filter           d_tag                          REFERENCES t_gmos_north_filter(c_tag),
  c_fpu              d_tag                          REFERENCES t_gmos_north_fpu(c_tag),
  c_x_binning        d_tag                 NOT NULL REFERENCES t_gmos_binning(c_tag),
  c_y_binning        d_tag                 NOT NULL REFERENCES t_gmos_binning(c_tag),
  c_wavelength_range d_wavelength_pm_range,
  c_disperser_order  d_tag                          REFERENCES t_gmos_disperser_order(c_tag),
  c_amp_gain         d_tag                 NOT NULL REFERENCES t_gmos_amp_gain(c_tag),

  CONSTRAINT check_disperser_config CHECK (
         (c_disperser IS NULL) = (c_wavelength_range IS NULL)
     AND (c_disperser IS NULL) = (c_disperser_order  IS NULL)
  ),

  -- Instrument configuration value, not considered part of the search key.
  c_exposure_time    interval              NOT NULL,
  CONSTRAINT check_non_negative_exposure_time CHECK (c_exposure_time >= interval '0 seconds')

);