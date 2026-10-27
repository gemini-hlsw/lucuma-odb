DROP VIEW v_flamingos_2_dynamic;

ALTER TABLE t_flamingos_2_dynamic
  ADD COLUMN c_window_cover d_tag NULL REFERENCES t_f2_window_cover(c_tag);

UPDATE t_flamingos_2_dynamic d
   SET c_window_cover = 'Close'
  FROM t_step s
 WHERE s.c_step_id = d.c_step_id
   AND s.c_step_type = 'dark'
   AND d.c_window_cover IS NULL
   AND NOT EXISTS (
     SELECT 1 FROM t_step_execution e WHERE e.c_step_id = d.c_step_id
   );

CREATE VIEW v_flamingos_2_dynamic AS
  SELECT t.*,
  f.c_wavelength AS c_central_wavelength,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL                                THEN t.c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_step_id END AS c_fpu_id
FROM
  t_flamingos_2_dynamic t
INNER JOIN t_f2_filter f ON t.c_filter = f.c_tag;
