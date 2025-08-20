-- ******************************
-- t_chron_asterism_target_update
-- ******************************

ALTER TABLE t_chron_asterism_target_update
  ALTER COLUMN c_timestamp TYPE timestamp USING c_timestamp AT TIME ZONE 'UTC';

-- ************************
-- t_chron_conditions_entry
-- ************************

DROP VIEW v_chron_conditions_entry;

ALTER TABLE t_chron_conditions_entry
  ALTER COLUMN c_timestamp TYPE timestamp USING c_timestamp AT TIME ZONE 'UTC';
  
CREATE OR REPLACE VIEW v_chron_conditions_entry AS
  SELECT *,
  CASE WHEN num_nulls(c_measurement_source, c_measurement_seeing, c_measurement_extinction_millimags, c_measurement_wavelength, c_measurement_azimuth, c_measurement_elevation) < 6 THEN c_chron_id END AS c_measurement_id,
  CASE WHEN num_nulls(c_intuition_expectation, c_intuition_timespan, c_intuition_seeing_trend) < 3 THEN c_chron_id END AS c_intuition_id,
  CASE WHEN num_nulls(c_intuition_expectation, c_intuition_timespan) < 2 THEN c_chron_id END AS c_expectation_id,
  CASE WHEN c_measurement_wavelength IS NOT NULL THEN c_chron_id END AS c_wavelength_id,
  CASE WHEN c_measurement_seeing IS NOT NULL THEN c_chron_id END AS c_measurement_seeing_id,
  CASE WHEN c_measurement_azimuth IS NOT NULL THEN c_chron_id END AS c_measurement_pointing_id
  FROM t_chron_conditions_entry;

-- **********************
-- t_chron_dataset_update
-- **********************

DROP VIEW v_chron_dataset_update;

ALTER TABLE t_chron_dataset_update
  ALTER COLUMN c_timestamp TYPE timestamp USING c_timestamp AT TIME ZONE 'UTC';

CREATE OR REPLACE VIEW v_chron_dataset_update AS
SELECT
  u.*,

  -- dataset interval from its constituent parts
  (u.c_mod_start_time OR u.c_mod_end_time)     AS c_mod_interval,

  COALESCE(u.c_new_start_time, d.c_start_time) AS c_coal_start_time,
  COALESCE(u.c_new_end_time,   d.c_end_time)   AS c_coal_end_time
FROM
  t_chron_dataset_update u
INNER JOIN
  t_dataset d ON d.c_dataset_id = u.c_dataset_id;

-- **********************
-- t_chron_program_update
-- **********************

ALTER TABLE t_chron_program_update
  ALTER COLUMN c_timestamp TYPE timestamp USING c_timestamp AT TIME ZONE 'UTC';