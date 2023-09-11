-- Let's drop the itc values, they will be recalculated soon
TRUNCATE TABLE t_itc_result;

-- Rename columns to indicaate whether they are science or acquisition
ALTER TABLE t_itc_result
  RENAME COLUMN c_exposure_time TO c_sci_exposure_time;

ALTER TABLE t_itc_result
  RENAME COLUMN c_exposure_count TO c_sci_exposure_count;

ALTER TABLE t_itc_result
  RENAME COLUMN c_signal_to_noise TO c_sci_signal_to_noise;

ALTER TABLE t_itc_result
  RENAME COLUMN c_acquisition_time TO c_acq_exposure_time;

-- Add missig acquisition columns
ALTER TABLE t_itc_result
  ADD COLUMN c_acq_exposure_count integer NOT NULL;

ALTER TABLE t_itc_result
  ADD COLUMN c_acq_signal_to_noise numeric(10, 3) NOT NULL;
