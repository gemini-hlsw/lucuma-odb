-- Spec-phot standards now default to time and count (see SpecPhotoExposureTime):
-- 1 x 120s through a long slit, 1 x 300s through the IFU. Convert the signal to
-- noise ETMs of standards that have not started executing, keeping their
-- wavelength. "Started" mirrors c_execution_state in v_generator_params.
UPDATE t_exposure_time_mode e
   SET c_exposure_time_mode = 'time_and_count',
       c_signal_to_noise    = NULL,
       c_exposure_time      = CASE
                                WHEN o.c_observing_mode_type IN ('gmos_north_ifu', 'gmos_south_ifu')
                                THEN interval '300 seconds'
                                ELSE interval '120 seconds'
                              END,
       c_exposure_count     = 1
  FROM t_observation o
 WHERE e.c_observation_id = o.c_observation_id
   AND o.c_calibration_role = 'spectrophotometric'
   AND o.c_observing_mode_type IN (
         'gmos_north_long_slit', 'gmos_south_long_slit',
         'gmos_north_ifu', 'gmos_south_ifu'
       )
   AND e.c_role IN ('requirement', 'science')
   AND e.c_exposure_time_mode = 'signal_to_noise'
   AND CASE
         WHEN o.c_declared_state IS NOT NULL
         THEN o.c_declared_state NOT IN ('ongoing', 'completed')
         ELSE NOT EXISTS (
           SELECT 1
             FROM t_execution_event v
            WHERE v.c_observation_id = o.c_observation_id
              AND v.c_event_type != 'slew'
         )
       END;
