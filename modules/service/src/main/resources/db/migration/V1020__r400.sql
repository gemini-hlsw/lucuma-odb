-- Insert the new G5310 disperser entry, borrowing the values from the old one.
INSERT INTO t_gmos_north_disperser (
  c_tag,
  c_short_name,
  c_long_name,
  c_ruling_density,
  c_dispersion_pm,
  c_simultaneous_coverage_nm,
  c_blaze_wavelength_nm,
  c_reference_resolution,
  c_wavelength_dither_nm
) SELECT
  'R400_G5310',
  c_short_name,
  'R400_G5310',
  c_ruling_density,
  c_dispersion_pm,
  c_simultaneous_coverage_nm,
  c_blaze_wavelength_nm,
  c_reference_resolution,
  c_wavelength_dither_nm
FROM t_gmos_north_disperser
WHERE c_tag = 'R400_G5305';

-- Update the instrument matrix to use the new entry instead of the old one.
UPDATE t_spectroscopy_config_option_gmos_north
   SET c_grating = 'R400_G5310'
 WHERE c_grating = 'R400_G5305';

-- Update the Smart Gcal table.
UPDATE t_smart_gmos_north
   SET c_disperser = 'R400_G5310'
 WHERE c_disperser = 'R400_G5305';

-- Update configuration requests.
UPDATE t_configuration_request
   SET c_gmos_north_longslit_grating = 'R400_G5310'
 WHERE c_gmos_north_longslit_grating = 'R400_G5305';

-- Update dynamic configs.
UPDATE t_gmos_north_dynamic
   SET c_grating_disperser = 'R400_G5310'
 WHERE c_grating_disperser = 'R400_G5305';

-- Update observing modes.
UPDATE t_gmos_north_long_slit
   SET c_grating = 'R400_G5310'
 WHERE c_grating = 'R400_G5305';

UPDATE t_gmos_north_long_slit
   SET c_initial_grating = 'R400_G5310'
 WHERE c_initial_grating = 'R400_G5305';

-- Delete the old G5305 disperser.
DELETE FROM t_gmos_north_disperser WHERE c_tag = 'R400_G5305';

