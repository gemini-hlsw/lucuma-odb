-- Redefine the GMOS dynamic step views in order to compute the central
-- wavelength.

DROP VIEW v_gmos_north_dynamic;

CREATE VIEW v_gmos_north_dynamic AS
  SELECT t.*,
  CASE WHEN t.c_grating_disperser        IS NOT NULL                                THEN t.c_step_id END AS c_grating_id,
  CASE WHEN t.c_fpu_custom_mask_filename IS NOT NULL                                THEN t.c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_filename IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_step_id END AS c_fpu_id,
  CASE WHEN t.c_grating_wavelength       IS NOT NULL OR t.c_filter      IS NOT NULL THEN t.c_step_id END AS c_central_wavelength_id,
  COALESCE (
    t.c_grating_wavelength,
    (SELECT f.c_wavelength FROM t_gmos_north_filter f WHERE f.c_tag = t.c_filter)
  ) AS c_central_wavelength
FROM
  t_gmos_north_dynamic t;

DROP VIEW v_gmos_south_dynamic;

CREATE VIEW v_gmos_south_dynamic AS
SELECT t.*,
  CASE WHEN t.c_grating_disperser        IS NOT NULL                                THEN t.c_step_id END AS c_grating_id,
  CASE WHEN t.c_fpu_custom_mask_filename IS NOT NULL                                THEN t.c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_filename IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_step_id END AS c_fpu_id,
  CASE WHEN t.c_grating_wavelength       IS NOT NULL OR t.c_filter      IS NOT NULL THEN t.c_step_id END AS c_central_wavelength_id,
  COALESCE (
    t.c_grating_wavelength,
    (SELECT f.c_wavelength FROM t_gmos_south_filter f WHERE f.c_tag = t.c_filter)
  ) AS c_central_wavelength
FROM
  t_gmos_south_dynamic t;