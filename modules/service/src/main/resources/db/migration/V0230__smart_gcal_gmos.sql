
DELETE FROM t_gmos_north_fpu WHERE c_tag LIKE 'Ifu_';
INSERT INTO t_gmos_north_fpu values ('Ifu2Slits', 'IFU-2', 'IFU 2 Slits', null, 33500000);
INSERT INTO t_gmos_north_fpu values ('IfuBlue', 'IFU-B', 'IFU Left Slit (blue)', null, 31750000);
INSERT INTO t_gmos_north_fpu values ('IfuRed', 'IFU-R', 'IFU Right Slit (red)', null, 35250000);

CREATE TABLE t_smart_gmos_north (
  c_smart_id         SERIAL                PRIMARY KEY,

  -- When there are multiple Gcal configuration values matching the same key,
  -- the step index is used to order them.  If two or more have the same step
  -- index, they may be executed in any order.
  c_step_index       smallint              NOT NULL DEFAULT 0,

  -- Gcal Configuration Value
  c_gcal_continuum   d_tag                          REFERENCES t_gcal_continuum(c_tag) ON DELETE CASCADE,
  c_gcal_ar_arc      boolean               NOT NULL DEFAULT FALSE,
  c_gcal_cuar_arc    boolean               NOT NULL DEFAULT FALSE,
  c_gcal_thar_arc    boolean               NOT NULL DEFAULT FALSE,
  c_gcal_xe_arc      boolean               NOT NULL DEFAULT FALSE,

  -- NOTE: Either a continuum lamp or else one of the arcs, but never both.
  CONSTRAINT check_lamp CHECK ((c_gcal_continuum IS NULL) = (c_gcal_ar_arc OR c_gcal_cuar_arc OR c_gcal_thar_arc OR c_gcal_xe_arc)),

  c_gcal_filter      d_tag                 NOT NULL REFERENCES t_gcal_filter(c_tag)   ON DELETE CASCADE,
  c_gcal_diffuser    d_tag                 NOT NULL REFERENCES t_gcal_diffuser(c_tag) ON DELETE CASCADE,
  c_gcal_shutter     d_tag                 NOT NULL REFERENCES t_gcal_shutter(c_tag)  ON DELETE CASCADE,

  -- GMOS North Configuration Value
  c_exposure_time    interval              NOT NULL,

  -- Search Key Definition
  c_gcal_lamp_type   e_gcal_lamp_type      NOT NULL GENERATED ALWAYS AS (CASE WHEN c_gcal_continuum IS NULL THEN 'Arc' :: e_gcal_lamp_type ELSE 'Flat' :: e_gcal_lamp_type END) STORED,
  c_gcal_baseline    e_gcal_baseline_type  NOT NULL,
  c_disperser        d_tag                          REFERENCES t_gmos_north_disperser(c_tag)  ON DELETE CASCADE,
  c_filter           d_tag                          REFERENCES t_gmos_north_filter(c_tag)     ON DELETE CASCADE,
  c_fpu              d_tag                          REFERENCES t_gmos_north_fpu(c_tag)        ON DELETE CASCADE,
  c_x_binning        d_tag                 NOT NULL REFERENCES t_gmos_binning(c_tag)          ON DELETE CASCADE,
  c_y_binning        d_tag                 NOT NULL REFERENCES t_gmos_binning(c_tag)          ON DELETE CASCADE,
  c_wavelength_range d_wavelength_pm_range NOT NULL,
  c_disperser_order  d_tag                 NOT NULL REFERENCES t_gmos_disperser_order(c_tag)  ON DELETE CASCADE,
  c_amp_gain         d_tag                 NOT NULL REFERENCES t_gmos_amp_gain(c_tag)         ON DELETE CASCADE
);