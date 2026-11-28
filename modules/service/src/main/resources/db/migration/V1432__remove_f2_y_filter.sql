-- The Flamingos 2 Y filter was never supported and is removed from lucuma-core.
-- Observations using it move to J.  Recorded execution history is not rewritten.

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM t_flamingos_2_dynamic WHERE c_filter = 'Y') THEN
    RAISE EXCEPTION 'Flamingos 2 Y filter appears in recorded steps, cannot remove it';
  END IF;
END $$;

-- Imaging: drop Y where J is already in the list, otherwise rename Y to J.
WITH dropped AS (
  DELETE FROM t_flamingos_2_imaging_filter y
   WHERE y.c_filter = 'Y'
     AND EXISTS (
       SELECT 1
         FROM t_flamingos_2_imaging_filter j
        WHERE j.c_observation_id = y.c_observation_id
          AND j.c_version        = y.c_version
          AND j.c_filter         = 'J'
     )
  RETURNING y.c_exposure_time_mode_id
)
DELETE FROM t_exposure_time_mode
 WHERE c_exposure_time_mode_id IN (SELECT c_exposure_time_mode_id FROM dropped);

UPDATE t_flamingos_2_imaging_filter SET c_filter = 'J' WHERE c_filter = 'Y';

UPDATE t_flamingos_2_long_slit SET c_filter             = 'J' WHERE c_filter             = 'Y';
UPDATE t_flamingos_2_long_slit SET c_initial_filter     = 'J' WHERE c_initial_filter     = 'Y';
UPDATE t_flamingos_2_long_slit SET c_acquisition_filter = 'J' WHERE c_acquisition_filter = 'Y';

UPDATE t_flamingos_2_mos SET c_filter             = 'J' WHERE c_filter             = 'Y';
UPDATE t_flamingos_2_mos SET c_initial_filter     = 'J' WHERE c_initial_filter     = 'Y';
UPDATE t_flamingos_2_mos SET c_acquisition_filter = 'J' WHERE c_acquisition_filter = 'Y';

UPDATE t_flamingos_2_daytime_calibration_setup SET c_filter = 'J' WHERE c_filter = 'Y';

-- Keep the array canonical: distinct and sorted.
UPDATE t_configuration_request
   SET c_flamingos_2_imaging_filters = (
     SELECT array_agg(t ORDER BY t COLLATE "C")
       FROM (SELECT DISTINCT unnest(array_replace(c_flamingos_2_imaging_filters, 'Y'::d_tag, 'J'::d_tag)) AS t) u
   )
 WHERE 'Y' = ANY (c_flamingos_2_imaging_filters);

DELETE FROM t_imaging_config_option_flamingos_2 WHERE c_filter = 'Y';
DELETE FROM t_imaging_config_option
 WHERE c_instrument = 'Flamingos2'
   AND NOT EXISTS (
     SELECT 1
       FROM t_imaging_config_option_flamingos_2 f
      WHERE f.c_instrument = t_imaging_config_option.c_instrument
        AND f.c_index      = t_imaging_config_option.c_index
   );

UPDATE t_spectroscopy_config_option_f2 SET c_filter = NULL WHERE c_filter = 'Y';

-- Smart gcal is reloaded from the CSV by R__SmartFlamingos2.
DELETE FROM t_smart_flamingos2 WHERE c_filter = 'Y';

DELETE FROM t_f2_filter WHERE c_tag = 'Y';
