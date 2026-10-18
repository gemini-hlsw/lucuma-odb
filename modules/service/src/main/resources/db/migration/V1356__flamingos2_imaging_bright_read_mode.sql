-- Flamingos2 imaging defaults to the Bright read mode regardless of exposure time.
ALTER TABLE t_flamingos_2_imaging
  ALTER COLUMN c_read_mode_default SET DEFAULT 'Bright';

-- Leave observations that have started executing untouched.
UPDATE t_flamingos_2_imaging i
   SET c_read_mode_default = 'Bright'
 WHERE i.c_read_mode_default <> 'Bright'
   AND NOT EXISTS (SELECT 1 FROM t_visit v WHERE v.c_observation_id = i.c_observation_id)
   AND NOT EXISTS (SELECT 1 FROM t_execution_event e WHERE e.c_observation_id = i.c_observation_id);
