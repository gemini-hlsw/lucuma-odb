-- Smart gcal table for GNIRS spectroscopy.  A search is performed using items
-- from the search key columns and the corresponding gcal config in t_gcal is
-- retrieved for the matching row(s).  The data itself is loaded by the
-- repeatable R__SmartGnirs migration.
CREATE TABLE t_smart_gnirs (

  -- Primary key, which is also a foreign key into t_gcal for the matching GCal
  -- configuration.  We search this table by the instrument config (the search
  -- key below) rather than by primary key; this key just links the instrument
  -- row to the matching gcal row in t_gcal.
  c_instrument       d_tag NOT NULL,
  c_gcal_id          int4  NOT NULL,
  PRIMARY KEY (c_instrument, c_gcal_id),

  FOREIGN KEY (c_instrument, c_gcal_id) REFERENCES t_gcal(c_instrument, c_gcal_id),
  CONSTRAINT check_is_gnirs CHECK (c_instrument = 'Gnirs'),

  -- GNIRS spectroscopy search key.  All components are present for every
  -- (spectroscopy) row; the central wavelength is stored as a range that the
  -- search wavelength must fall within.  Indexes on the disperser, cross
  -- disperser and fpu are created after the table is initially loaded.
  c_pixel_scale      varchar               NOT NULL,
  c_disperser        d_tag                 NOT NULL REFERENCES t_gnirs_grating(c_tag),
  c_cross_dispersed  d_tag                 NOT NULL REFERENCES t_gnirs_prism(c_tag),
  c_wavelength_range d_wavelength_pm_range NOT NULL,
  c_fpu              d_tag                 NOT NULL REFERENCES t_gnirs_fpu_slit(c_tag),
  c_well_depth       e_gnirs_well_depth    NOT NULL,

  -- When there are multiple Gcal configuration values matching the same key,
  -- the step order is used to order them.  If two or more have the same step
  -- order, they may be executed in any order.
  c_step_order       int8                  NOT NULL DEFAULT 0,
  CONSTRAINT check_positive_step_order CHECK (c_step_order > 0),

  -- Instrument configuration value, not considered part of the search key.
  c_exposure_time    interval              NOT NULL,
  CONSTRAINT check_non_negative_exposure_time CHECK (c_exposure_time >= interval '0 seconds')

);
