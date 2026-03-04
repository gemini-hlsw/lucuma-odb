-- Smart gcal table for IGRINS-2.
-- IGRINS-2 is a spectrograph with no configurable items
CREATE TABLE t_smart_igrins_2 (

  c_instrument       d_tag    NOT NULL,
  PRIMARY KEY (c_instrument, c_gcal_id),
  FOREIGN KEY (c_instrument, c_gcal_id)
    REFERENCES t_gcal(c_instrument, c_gcal_id),
  CONSTRAINT check_is_igrins2 CHECK (c_instrument = 'Igrins2'),

  c_gcal_id          int4     NOT NULL,

  c_step_order       int8     NOT NULL DEFAULT 0,
  CONSTRAINT check_positive_step_order CHECK (c_step_order > 0),

  c_exposure_time    interval NOT NULL,
  CONSTRAINT check_non_negative_exposure_time CHECK (c_exposure_time >= interval '0 seconds')

);
