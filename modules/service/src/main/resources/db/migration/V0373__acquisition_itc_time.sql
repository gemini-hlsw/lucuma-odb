-- Let's drop the itc values, they will be recalculated soon
TRUNCATE TABLE t_itc_result;

-- Add a column to store the acquisition time of the ITC measurement
ALTER TABLE t_itc_result
  ADD COLUMN c_acquisition_time interval NOT NULL;
