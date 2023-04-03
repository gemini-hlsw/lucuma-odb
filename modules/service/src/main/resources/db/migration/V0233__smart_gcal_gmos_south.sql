-- Smart gcal table for GMOS South.  A search is performed using items from the
-- search key columns and the corresponding gcal config in t_gcal is retrieved
-- for the matching row(s).
CREATE TABLE t_smart_gmos_south (

  -- Primary key, which is also a foreign key into t_gcal for the matching GCal
  -- configuration.  We don't really search this table by primary key but rather
  -- by the instrument config (see Search Key below).  This key just links the
  -- instrument row to the matching gcal row in t_gcal.
  c_instrument       d_tag NOT NULL,
  c_gcal_id          int4  NOT NULL,
  PRIMARY KEY (c_instrument, c_gcal_id),

  FOREIGN KEY (c_instrument, c_gcal_id) REFERENCES t_gcal(c_instrument, c_gcal_id),
  CONSTRAINT check_is_gmos_south CHECK (c_instrument = 'GmosSouth'),

  -- GMOS South Search Key.  We find smart gcal rows via lookups on the full
  -- search key.  An index of the first three columns is created after the
  -- table is initially loaded.
  c_disperser        d_tag                          REFERENCES t_gmos_south_disperser(c_tag),
  c_filter           d_tag                          REFERENCES t_gmos_south_filter(c_tag),
  c_fpu              d_tag                          REFERENCES t_gmos_south_fpu(c_tag),
  c_x_binning        d_tag                 NOT NULL REFERENCES t_gmos_binning(c_tag),
  c_y_binning        d_tag                 NOT NULL REFERENCES t_gmos_binning(c_tag),
  c_wavelength_range d_wavelength_pm_range,
  c_disperser_order  d_tag                          REFERENCES t_gmos_disperser_order(c_tag),
  c_amp_gain         d_tag                 NOT NULL REFERENCES t_gmos_amp_gain(c_tag),

  -- There is only a wavelength range and disperser order if there is in fact
  -- a disperser defined.  Otherwise this is an imaging row.
  CONSTRAINT check_disperser_config CHECK (
         (c_disperser IS NULL) = (c_wavelength_range IS NULL)
     AND (c_disperser IS NULL) = (c_disperser_order  IS NULL)
  ),

  -- When there are multiple Gcal configuration values matching the same key,
  -- the step order is used to order them.  If two or more have the same step
  -- order, they may be executed in any order.
  c_step_order       int8                  NOT NULL DEFAULT 0,
  CONSTRAINT check_positive_step_order CHECK (c_step_order > 0),

  -- Instrument configuration value, not considered part of the search key.
  c_exposure_time    interval              NOT NULL,
  CONSTRAINT check_non_negative_exposure_time CHECK (c_exposure_time >= interval '0 seconds')

);