ALTER TABLE t_f2_filter
  ALTER COLUMN c_wavelength SET NOT NULL;

DROP VIEW v_flamingos_2_dynamic;

CREATE VIEW v_flamingos_2_dynamic AS
  SELECT t.*,
  f.c_wavelength AS c_central_wavelength,
  CASE WHEN t.c_fpu_custom_mask_filename IS NOT NULL                                THEN t.c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_filename IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_step_id END AS c_fpu_id
FROM
  t_flamingos_2_dynamic t
INNER JOIN t_f2_filter f ON t.c_filter = f.c_tag;