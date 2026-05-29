-- `centralWavelength` is now a computed member of `GnirsDynamicConfig` rather than a
-- stored field.  As with the GMOS dynamic views, derive it in `v_gnirs_dynamic`
-- instead of persisting it in `t_gnirs_dynamic`:
--
--   * acquisition mirror "out" (spectroscopy)  -> the grating wavelength
--   * acquisition mirror "in"                  -> the filter's central wavelength
--
-- The filter's central wavelength is `optimalWavelength`, which is null only for the
-- cross-dispersed filter; the model falls back to 1.65µm in that case, so the view
-- does the same with a final COALESCE literal.

DROP VIEW v_gnirs_dynamic;

ALTER TABLE t_gnirs_dynamic DROP COLUMN c_central_wavelength;

CREATE VIEW v_gnirs_dynamic AS
  SELECT t.*,
  CASE WHEN t.c_prism IS NOT NULL THEN t.c_step_id END AS c_acquisition_mirror_out_id,
  COALESCE(
    t.c_grating_wavelength,
    (SELECT f.c_wavelength FROM t_gnirs_filter f WHERE f.c_tag = t.c_filter),
    1650000::d_wavelength_pm
  ) AS c_central_wavelength
FROM
  t_gnirs_dynamic t;
