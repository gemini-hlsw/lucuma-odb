-- Delete twilight calibration observations they will be regenerated
DELETE FROM t_observation
WHERE c_calibration_role = 'twilight';
