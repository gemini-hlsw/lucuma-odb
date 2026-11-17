-- GMOS MOS science now takes a Twilight through its mask and a long slit
-- specphot.  Queue one calibration recalculation per program with an
-- uncompleted GMOS MOS observation so existing programs get them.
DO $$
DECLARE
  r record;
BEGIN
  FOR r IN
    SELECT DISTINCT ON (o.c_program_id) o.c_observation_id, o.c_program_id
    FROM (
      SELECT c_observation_id FROM t_gmos_north_mos
      UNION ALL
      SELECT c_observation_id FROM t_gmos_south_mos
    ) m
    JOIN t_observation o ON o.c_observation_id = m.c_observation_id
    JOIN v_execution_state e ON e.c_observation_id = o.c_observation_id
    WHERE o.c_calibration_role IS NULL
      AND o.c_existence = 'present'
      AND o.c_workflow_user_state IS DISTINCT FROM 'inactive'
      AND e.c_execution_state NOT IN ('completed', 'declared_complete')
    ORDER BY o.c_program_id, o.c_observation_id
  LOOP
    CALL invalidate_calibration_calc(r.c_observation_id, r.c_program_id, 'recalc');
  END LOOP;
END $$;
