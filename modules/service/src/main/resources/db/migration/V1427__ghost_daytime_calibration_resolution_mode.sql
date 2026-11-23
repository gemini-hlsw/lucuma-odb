-- GHOST daytime calibrations choose their resolution mode, which was fixed
-- at standard.

ALTER TABLE t_daytime_calibration
  ADD COLUMN c_ghost_resolution_mode d_tag NULL REFERENCES t_ghost_resolution_mode(c_tag);

UPDATE t_daytime_calibration
   SET c_ghost_resolution_mode = 'standard'
 WHERE c_instrument = 'Ghost';

-- Run the deferred triggers the update queued, or the ALTER TABLE below fails.
SET CONSTRAINTS ALL IMMEDIATE;

ALTER TABLE t_daytime_calibration
  ADD CONSTRAINT t_daytime_calibration_ghost_resolution_mode_check
    CHECK ((c_ghost_resolution_mode IS NOT NULL) = (c_instrument = 'Ghost'));

-- c_ghost_id is the key of the nested ghost object, null for other instruments.
CREATE OR REPLACE VIEW v_daytime_calibration AS
  SELECT
    c_observation_id,
    c_instrument,
    c_observing_mode_type,
    c_ghost_resolution_mode,
    CASE WHEN c_instrument = 'Ghost' THEN c_observation_id END AS c_ghost_id
  FROM t_daytime_calibration;
