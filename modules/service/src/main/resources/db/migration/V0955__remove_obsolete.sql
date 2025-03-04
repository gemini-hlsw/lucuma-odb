-- Remove GMOS North u' filter and replace with g'.

UPDATE t_gmos_north_long_slit
   SET c_filter = 'GPrime'
 WHERE c_filter = 'UPrime';

UPDATE t_gmos_north_long_slit
   SET c_initial_filter = 'GPrime'
 WHERE c_initial_filter = 'UPrime';

UPDATE t_gmos_north_dynamic
   SET c_filter = 'GPrime'
 WHERE c_filter = 'UPrime';

DELETE FROM t_smart_gmos_north
  WHERE c_filter = 'UPrime';

DELETE FROM t_gmos_north_filter
  WHERE c_tag = 'UPrime';

-- Remove GMOS North B600_G5303 grating and replace with B600_G5307.

UPDATE t_gmos_north_long_slit
   SET c_grating = 'B600_G5307'
 WHERE c_grating = 'B600_G5303';

UPDATE t_gmos_north_long_slit
   SET c_initial_grating = 'B600_G5307'
 WHERE c_initial_grating = 'B600_G5303';

UPDATE t_gmos_north_dynamic
   SET c_grating_disperser = 'B600_G5307'
 WHERE c_grating_disperser = 'B600_G5303';

DELETE FROM t_configuration_request
  WHERE c_gmos_north_longslit_grating = 'B600_G5303';

DELETE FROM t_smart_gmos_north
  WHERE c_disperser = 'B600_G5303';

DELETE FROM t_gmos_north_disperser
  WHERE c_tag = 'B600_G5303';

-- Remove GMOS North R150_G5306 grating and replace with R150_G5308.

UPDATE t_gmos_north_long_slit
   SET c_grating = 'R150_G5308'
 WHERE c_grating = 'R150_G5306';

UPDATE t_gmos_north_long_slit
   SET c_initial_grating = 'R150_G5308'
 WHERE c_initial_grating = 'R150_G5306';

UPDATE t_gmos_north_dynamic
   SET c_grating_disperser = 'R150_G5308'
 WHERE c_grating_disperser = 'R150_G5306';

DELETE FROM t_configuration_request
  WHERE c_gmos_north_longslit_grating = 'R150_G5306';

DELETE FROM t_smart_gmos_north
  WHERE c_disperser = 'R150_G5306';

DELETE FROM t_gmos_north_disperser
  WHERE c_tag = 'R150_G5306';

-- Remove GMOS North stage mode FollowXyz, FollowZ and replace with FollowXy

UPDATE t_gmos_north_static
   SET c_stage_mode = 'FollowXy'
 WHERE c_stage_mode = 'FollowXyz'
    OR c_stage_mode = 'FollowZ';

DELETE FROM t_gmos_north_stage_mode
   WHERE c_tag = 'FollowXyz'
      OR c_tag = 'FollowZ';

-- Remove GMOS ROI BottomSpectrum and TopSpectrum and replace with CentralSpectrum

UPDATE t_gmos_north_long_slit
   SET c_roi = 'CentralSpectrum'
 WHERE c_roi = 'BottomSpectrum'
    OR c_roi = 'TopSpectrum';

UPDATE t_gmos_south_long_slit
   SET c_roi = 'CentralSpectrum'
 WHERE c_roi = 'BottomSpectrum'
    OR c_roi = 'TopSpectrum';

UPDATE t_gmos_north_dynamic
   SET c_roi = 'CentralSpectrum'
 WHERE c_roi = 'BottomSpectrum'
    OR c_roi = 'TopSpectrum';

UPDATE t_gmos_south_dynamic
   SET c_roi = 'CentralSpectrum'
 WHERE c_roi = 'BottomSpectrum'
    OR c_roi = 'TopSpectrum';

DELETE FROM t_gmos_readout
  WHERE c_roi = 'BottomSpectrum'
     OR c_roi = 'TopSpectrum';

DELETE FROM t_gmos_roi
  WHERE c_tag = 'BottomSpectrum'
     OR c_tag = 'TopSpectrum';

-- Remove GMOS South Lya395 filter and replace with HeII.

UPDATE t_gmos_south_long_slit
   SET c_filter = 'HeII'
 WHERE c_filter = 'Lya395';

UPDATE t_gmos_south_long_slit
   SET c_initial_filter = 'HeII'
 WHERE c_initial_filter = 'Lya395';

UPDATE t_gmos_south_dynamic
   SET c_filter = 'HeII'
 WHERE c_filter = 'Lya395';

DELETE FROM t_smart_gmos_south
  WHERE c_filter = 'Lya395';

DELETE FROM t_gmos_south_filter
  WHERE c_tag = 'Lya395';

-- Remove GMOS South stage mode FollowXy and replace with FollowXyz

UPDATE t_gmos_south_static
   SET c_stage_mode = 'FollowXyz'
 WHERE c_stage_mode = 'FollowXy';

DELETE FROM t_gmos_south_stage_mode
   WHERE c_tag = 'FollowXy';
