
CREATE VIEW v_gmos_north_dynamic AS
  SELECT *,
  CASE WHEN c_grating_disperser        IS NOT NULL                              THEN c_step_id END AS c_grating_id,
  CASE WHEN c_fpu_custom_mask_filename IS NOT NULL                              THEN c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN c_fpu_custom_mask_filename IS NOT NULL OR c_fpu_builtin IS NOT NULL THEN c_step_id END AS c_fpu_id
  FROM t_gmos_north_dynamic;

CREATE VIEW v_gmos_south_dynamic AS
  SELECT *,
  CASE WHEN c_grating_disperser        IS NOT NULL                              THEN c_step_id END AS c_grating_id,
  CASE WHEN c_fpu_custom_mask_filename IS NOT NULL                              THEN c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN c_fpu_custom_mask_filename IS NOT NULL OR c_fpu_builtin IS NOT NULL THEN c_step_id END AS c_fpu_id
  FROM t_gmos_south_dynamic;