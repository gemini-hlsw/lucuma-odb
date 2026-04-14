-- reset exisiting observation with z' explicitly selected as an acquisition
-- filter to null
UPDATE t_gmos_north_long_slit
  SET c_acquisition_filter = NULL
  WHERE c_acquisition_filter = 'ZPrime';

UPDATE t_gmos_south_long_slit
  SET c_acquisition_filter = NULL
  WHERE c_acquisition_filter = 'ZPrime';

UPDATE t_gmos_north_filter
  SET c_is_acquisition_filter = FALSE
  WHERE c_tag = 'ZPrime';

UPDATE t_gmos_south_filter
  SET c_is_acquisition_filter = FALSE
  WHERE c_tag = 'ZPrime';
