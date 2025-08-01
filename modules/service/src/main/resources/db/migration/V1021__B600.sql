-- Update the instrument matrix to use the new entry instead of the old one.
DELETE FROM t_spectroscopy_config_option_gmos_south WHERE c_grating = 'B600_G5323';

-- Update the Smart Gcal table.
DELETE FROM t_smart_gmos_south WHERE c_disperser = 'B600_G5323';

-- Update configuration requests.
UPDATE t_configuration_request
   SET c_gmos_south_longslit_grating = 'R600_G5324'
 WHERE c_gmos_south_longslit_grating = 'B600_G5323';

-- Update dynamic configs.
UPDATE t_gmos_south_dynamic
   SET c_grating_disperser = 'R600_G5324'
 WHERE c_grating_disperser = 'B600_G5323';

-- Update observing modes.
UPDATE t_gmos_south_long_slit
   SET c_grating = 'R600_G5324'
 WHERE c_grating = 'B600_G5323';

UPDATE t_gmos_south_long_slit
   SET c_initial_grating = 'R600_G5324'
 WHERE c_initial_grating = 'R600_G5323';

-- Delete the old G5323 disperser.
DELETE FROM t_gmos_south_disperser WHERE c_tag = 'B600_G5323';