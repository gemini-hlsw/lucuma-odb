-- The Calibrations group also holds MOS mask flats.
UPDATE t_group
SET    c_calibration_roles = array_append(c_calibration_roles, 'mos_mask_flat')
WHERE  c_system
  AND  c_name = 'Calibrations'
  AND  NOT ('mos_mask_flat' = ANY (c_calibration_roles));

-- The mask file a MOS mask flat imaged.  A replaced file gets a new remote
-- path, which is how a flat taken of the previous plate is recognised.
CREATE TABLE t_mos_mask_flat (
  c_observation_id d_observation_id NOT NULL PRIMARY KEY
    REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_remote_path    text             NOT NULL
);

-- Replacing a mask file touches no observation, so queue a calibration
-- recalculation for the science observations using the mask.
CREATE OR REPLACE FUNCTION mos_mask_file_replaced()
RETURNS TRIGGER AS $$
DECLARE
  r record;
BEGIN
  FOR r IN
    SELECT o.c_observation_id, o.c_program_id
    FROM (
      SELECT c_observation_id, c_mask_attachment_id FROM t_gmos_north_mos
      UNION ALL
      SELECT c_observation_id, c_mask_attachment_id FROM t_gmos_south_mos
      UNION ALL
      SELECT c_observation_id, c_mask_attachment_id FROM t_flamingos_2_mos
    ) m
    JOIN t_observation o ON o.c_observation_id = m.c_observation_id
    WHERE m.c_mask_attachment_id = NEW.c_attachment_id
      AND o.c_calibration_role IS NULL
      AND o.c_existence = 'present'
  LOOP
    CALL invalidate_calibration_calc(r.c_observation_id, r.c_program_id, 'recalc');
  END LOOP;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER mos_mask_file_replaced_trigger
  AFTER UPDATE OF c_remote_path ON t_attachment
  FOR EACH ROW
  WHEN (NEW.c_attachment_type = 'mos_mask' AND NEW.c_remote_path IS DISTINCT FROM OLD.c_remote_path)
  EXECUTE FUNCTION mos_mask_file_replaced();
