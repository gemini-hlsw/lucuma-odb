-- GNIRS daytime calibrations choose their well depth, which was fixed at
-- shallow.

ALTER TABLE t_daytime_calibration
  ADD COLUMN c_gnirs_well_depth e_gnirs_well_depth NULL;

UPDATE t_daytime_calibration
   SET c_gnirs_well_depth = 'Shallow'
 WHERE c_instrument = 'Gnirs';

-- Run the deferred triggers the update queued, or the ALTER TABLE below fails.
SET CONSTRAINTS ALL IMMEDIATE;

ALTER TABLE t_daytime_calibration
  ADD CONSTRAINT t_daytime_calibration_gnirs_well_depth_check
    CHECK ((c_gnirs_well_depth IS NOT NULL) = (c_instrument = 'Gnirs'));

-- c_gnirs_id is the key of the nested gnirs object, null for other instruments.
CREATE OR REPLACE VIEW v_daytime_calibration AS
  SELECT
    c_observation_id,
    c_instrument,
    c_observing_mode_type,
    c_ghost_resolution_mode,
    CASE WHEN c_instrument = 'Ghost' THEN c_observation_id END AS c_ghost_id,
    c_gnirs_well_depth,
    CASE WHEN c_instrument = 'Gnirs' THEN c_observation_id END AS c_gnirs_id
  FROM t_daytime_calibration;
