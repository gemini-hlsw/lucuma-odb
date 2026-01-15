-- Adds entries in t_offset_generator for each GMOS North imaging observation
DO $$
DECLARE
  oid d_observation_id;
BEGIN
  FOR oid IN
    SELECT c_observation_id FROM t_gmos_north_imaging WHERE c_variant IN ('grouped', 'interleaved')
  LOOP
    IF NOT EXISTS (
      SELECT 1 FROM t_offset_generator WHERE c_observation_id = oid
    ) THEN
      INSERT INTO t_offset_generator (
        c_observation_id,
        c_role,
        c_type
      )
      VALUES (oid, 'object', 'none'), (oid, 'sky', 'none');
    END IF;
  END LOOP;
END $$;

-- Adds entries in t_offset_generator for each GMOS South imaging observation
DO $$
DECLARE
  oid d_observation_id;
BEGIN
  FOR oid IN
    SELECT c_observation_id FROM t_gmos_south_imaging WHERE c_variant IN ('grouped', 'interleaved')
  LOOP
    IF NOT EXISTS (
      SELECT 1 FROM t_offset_generator WHERE c_observation_id = oid
    ) THEN
      INSERT INTO t_offset_generator (
        c_observation_id,
        c_role,
        c_type
      )
      VALUES (oid, 'object', 'none'), (oid, 'sky', 'none');
    END IF;
  END LOOP;
END $$;