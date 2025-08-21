-- Add Zero preset and remove 1.5 for CloudExtinction

UPDATE t_observation
SET c_cloud_extinction = 'one_point_zero'
WHERE c_cloud_extinction = 'one_point_five';

UPDATE t_configuration_request
SET c_cloud_extinction = 'one_point_zero'
WHERE c_cloud_extinction = 'one_point_five';

DELETE FROM t_cloud_extinction WHERE c_tag = 'one_point_five';

-- Update the constraint to allow 0.0 values for the Zero preset
ALTER TABLE t_cloud_extinction DROP CONSTRAINT t_cloud_extinction_c_value_check;
ALTER TABLE t_cloud_extinction ADD CONSTRAINT t_cloud_extinction_c_value_check
CHECK (c_value >= 0.0 and c_value <= 3.0);

INSERT INTO t_cloud_extinction VALUES ('zero', '0.0', '0.0', 0.0);
