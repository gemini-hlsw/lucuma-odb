ALTER TABLE t_flamingos_2_dynamic
  ADD COLUMN c_decker d_tag REFERENCES t_f2_decker(c_tag);

UPDATE t_flamingos_2_dynamic
   SET c_decker = 'LongSlit';

UPDATE t_flamingos_2_dynamic
   SET c_readout_mode = 'Science'
 WHERE c_readout_mode IS NULL;

UPDATE t_flamingos_2_dynamic
   SET c_reads = 'reads_1'
 WHERE c_reads IS NULL;

 ALTER TABLE t_flamingos_2_dynamic
   ALTER COLUMN c_decker       SET NOT NULL,
   ALTER COLUMN c_readout_mode SET NOT NULL,
   ALTER COLUMN c_reads        SET NOT NULL;

-- Recreate the view
DROP VIEW v_flamingos_2_dynamic;

CREATE VIEW v_flamingos_2_dynamic AS
  SELECT t.*,
  CASE WHEN t.c_fpu_custom_mask_filename IS NOT NULL                                THEN t.c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_filename IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_step_id END AS c_fpu_id
FROM
  t_flamingos_2_dynamic t;