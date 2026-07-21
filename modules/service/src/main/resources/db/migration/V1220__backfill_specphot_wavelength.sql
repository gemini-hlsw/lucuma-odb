-- Backfill for the specphot S/N wavelength staleness bug.
--
-- Invalidate obs calc for one science observation per program that has a
-- non-executed spectrophotometric calibration, so that the calibrations
-- daemon will recalculate the S/N wavelength for those specphot
-- calibrations correctly.
DO $$
DECLARE
  r RECORD;
BEGIN
  FOR r IN
    SELECT DISTINCT ON (sci.c_program_id) sci.c_observation_id
    FROM t_observation sci
    LEFT JOIN v_generator_params sci_gp ON sci_gp.c_observation_id = sci.c_observation_id
    WHERE sci.c_calibration_role IS NULL
      AND (sci_gp.c_execution_state IS NULL OR sci_gp.c_execution_state NOT IN (
        'ongoing'::e_execution_state, 'completed'::e_execution_state, 'declared_complete'::e_execution_state
      ))
      AND EXISTS (
        SELECT 1
        FROM t_observation calib
        LEFT JOIN v_generator_params calib_gp ON calib_gp.c_observation_id = calib.c_observation_id
        WHERE calib.c_program_id = sci.c_program_id
          AND calib.c_calibration_role = 'spectrophotometric'::e_calibration_role
          AND (calib_gp.c_execution_state IS NULL OR calib_gp.c_execution_state NOT IN (
            'ongoing'::e_execution_state, 'completed'::e_execution_state, 'declared_complete'::e_execution_state
          ))
      )
    ORDER BY sci.c_program_id, sci.c_observation_id
  LOOP
    CALL invalidate_obscalc(r.c_observation_id);
  END LOOP;
END $$;
