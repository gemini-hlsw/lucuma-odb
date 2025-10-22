
CREATE TYPE e_gmos_long_slit_acquisition_roi AS enum(
  'Ccd2Stamp',
  'Ccd2',
  'Stamp',
  'FullCcd2'
);

-- Add a column to keep up with the explicitly specified GMOS North acquisition
-- ROI.
ALTER TABLE t_gmos_north_long_slit
  ADD COLUMN c_acquisition_roi e_gmos_long_slit_acquisition_roi;

ALTER TABLE t_gmos_south_long_slit
  ADD COLUMN c_acquisition_roi e_gmos_long_slit_acquisition_roi;

-- Recreate the view.

DROP VIEW v_gmos_north_long_slit;
DROP VIEW v_gmos_south_long_slit;

CREATE VIEW v_gmos_north_long_slit AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_north_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default,

  CASE
    WHEN o.c_calibration_role = 'spectrophotometric'::e_calibration_role THEN 'Stamp'::e_gmos_long_slit_acquisition_roi
    WHEN m.c_roi              = 'CentralSpectrum'                        THEN 'Ccd2Stamp'::e_gmos_long_slit_acquisition_roi
    ELSE 'Ccd2'::e_gmos_long_slit_acquisition_roi
  END AS c_acquisition_roi_default

FROM t_gmos_north_long_slit m
INNER JOIN t_observation o ON o.c_observation_id = m.c_observation_id;

CREATE VIEW v_gmos_south_long_slit AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_south_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default,

  CASE
    WHEN o.c_calibration_role = 'spectrophotometric'::e_calibration_role THEN 'Stamp'::e_gmos_long_slit_acquisition_roi
    WHEN m.c_roi              = 'CentralSpectrum'                        THEN 'Ccd2Stamp'::e_gmos_long_slit_acquisition_roi
    ELSE 'Ccd2'::e_gmos_long_slit_acquisition_roi
  END AS c_acquisition_roi_default

FROM t_gmos_south_long_slit m
INNER JOIN t_observation o ON o.c_observation_id = m.c_observation_id;