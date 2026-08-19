-- The GNIRS acquisition signal-to-noise now follows the ITC brightness
-- classification (Very Bright 30, Bright 20, Faint 10) instead of being fixed at 10.
--
-- The classification only exists after an ITC pass, well after the observing mode is
-- created, so the acquisition exposure time mode row becomes "explicit or derived":
-- when c_is_explicit is false the ITC service owns c_signal_to_noise and rewrites it
-- from the classification.  The row is always present, so the ETM row counts that
-- check_etm_consistent enforces are unchanged.

ALTER TABLE t_exposure_time_mode
  ADD COLUMN c_is_explicit boolean NOT NULL DEFAULT true;

COMMENT ON COLUMN t_exposure_time_mode.c_is_explicit IS
  'True when the user set this exposure time mode, false when it is derived and may be '
  'rewritten automatically.  Only meaningful for c_role = ''acquisition'' on the GNIRS '
  'observing modes; true everywhere else, which is why true is the default.';

-- A derived acquisition is always signal-to-noise: the ITC sizes it, and
-- AcquisitionConfig.resolvedCoadds relies on the mode to decide where coadds come from.
ALTER TABLE t_exposure_time_mode
  ADD CONSTRAINT t_exposure_time_mode_derived_is_sn
    CHECK (c_is_explicit OR c_exposure_time_mode = 'signal_to_noise'::e_exp_time_mode);

-- Recreate v_exposure_time_mode.  It selects e.*, so the new column would be inserted
-- ahead of the existing synthetic id columns and CREATE OR REPLACE VIEW would reject the
-- column reordering.  The three new c_explicit_* ids are "null key => null object" keys
-- letting explicitExposureTimeMode resolve to null for a derived row.
DROP VIEW v_exposure_time_mode;

CREATE VIEW v_exposure_time_mode AS
  SELECT e.*,
  CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
  CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id,
  CASE WHEN e.c_is_explicit                            THEN e.c_exposure_time_mode_id END AS c_explicit_id,
  CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'signal_to_noise'
       THEN e.c_exposure_time_mode_id END AS c_explicit_signal_to_noise_id,
  CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'time_and_count'
       THEN e.c_exposure_time_mode_id END AS c_explicit_time_and_count_id
FROM t_exposure_time_mode e;

-- The GHOST channel views are deliberately identical in shape to v_exposure_time_mode
-- (they share BaseExposureTimeModeView), and they select e.* too, so they need the same
-- treatment.
DROP VIEW v_ghost_blue_exposure_time_mode;

CREATE VIEW v_ghost_blue_exposure_time_mode AS
  SELECT
    e.*,
    CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
    CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id,
    CASE WHEN e.c_is_explicit                            THEN e.c_exposure_time_mode_id END AS c_explicit_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'signal_to_noise'
         THEN e.c_exposure_time_mode_id END AS c_explicit_signal_to_noise_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'time_and_count'
         THEN e.c_exposure_time_mode_id END AS c_explicit_time_and_count_id
  FROM t_exposure_time_mode e
  INNER JOIN t_ghost_ifu g ON g.c_blue_exposure_time_mode_id = e.c_exposure_time_mode_id;

DROP VIEW v_ghost_red_exposure_time_mode;

CREATE VIEW v_ghost_red_exposure_time_mode AS
  SELECT
    e.*,
    CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
    CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id,
    CASE WHEN e.c_is_explicit                            THEN e.c_exposure_time_mode_id END AS c_explicit_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'signal_to_noise'
         THEN e.c_exposure_time_mode_id END AS c_explicit_signal_to_noise_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'time_and_count'
         THEN e.c_exposure_time_mode_id END AS c_explicit_time_and_count_id
  FROM t_exposure_time_mode e
  INNER JOIN t_ghost_ifu g ON g.c_red_exposure_time_mode_id = e.c_exposure_time_mode_id;

-- Clones must carry explicitness, or a cloned observation (or a telluric standard)
-- would silently turn a derived acquisition into an explicit one.
CREATE OR REPLACE FUNCTION clone_exposure_time_modes(
  old_obs_id d_observation_id,
  new_obs_id d_observation_id
)
RETURNS TABLE (
  old_exposure_time_mode_id integer,
  new_exposure_time_mode_id integer
)
LANGUAGE plpgsql
AS $$
BEGIN

  DROP TABLE IF EXISTS etm_map;

  CREATE TEMPORARY TABLE etm_map (
    rn         integer PRIMARY KEY,
    old_etm_id integer NOT NULL,
    new_etm_id integer
  ) ON COMMIT DROP;

  INSERT INTO etm_map (
    rn,
    old_etm_id
  )
  SELECT
    row_number() OVER (ORDER BY c_exposure_time_mode_id),
    c_exposure_time_mode_id
  FROM t_exposure_time_mode
  WHERE c_observation_id = old_obs_id;

  WITH inserted_etms AS (
    INSERT INTO t_exposure_time_mode(
      c_observation_id,
      c_role,
      c_exposure_time_mode,
      c_signal_to_noise,
      c_signal_to_noise_at,
      c_exposure_time,
      c_exposure_count,
      c_is_explicit
    )
    SELECT
      new_obs_id,
      c_role,
      c_exposure_time_mode,
      c_signal_to_noise,
      c_signal_to_noise_at,
      c_exposure_time,
      c_exposure_count,
      c_is_explicit
    FROM t_exposure_time_mode
    WHERE c_observation_id = old_obs_id
    ORDER BY c_exposure_time_mode_id
    RETURNING c_exposure_time_mode_id AS new_etm_id
  ),

  new_etm_ids_with_rn AS (
    SELECT
      row_number() OVER (ORDER BY new_etm_id) AS rn,
      new_etm_id
    FROM inserted_etms
    ORDER BY new_etm_id
  )

  UPDATE etm_map AS e
  SET new_etm_id = n.new_etm_id
  FROM new_etm_ids_with_rn AS n
  WHERE e.rn = n.rn;

  RETURN QUERY SELECT e.old_etm_id, e.new_etm_id FROM etm_map e;

END;
$$;

-- Existing GNIRS acquisition rows that are byte-identical to what
-- ExposureTimeMode.forAcquisition produced -- signal-to-noise 10 at one of the
-- observation's science wavelengths -- carry no information distinguishing "never
-- touched" from "deliberately typed 10", so treat them as derived.  Anything else stays
-- explicit.
--
-- ATTENTION: the 10 here is acquisitionSignalToNoise(Faint) from
-- modules/sequence/src/main/scala/lucuma/odb/sequence/gnirs/shared.scala.
UPDATE t_exposure_time_mode acq
   SET c_is_explicit = false
 WHERE acq.c_role = 'acquisition'::e_exposure_time_mode_role
   AND acq.c_exposure_time_mode = 'signal_to_noise'::e_exp_time_mode
   AND acq.c_signal_to_noise = 10
   AND EXISTS (
     SELECT 1
       FROM t_observation o
      WHERE o.c_observation_id = acq.c_observation_id
        AND o.c_observing_mode_type IN (
              'gnirs_long_slit'::e_observing_mode_type,
              'gnirs_ifu'::e_observing_mode_type,
              'gnirs_imaging'::e_observing_mode_type
            )
   )
   AND EXISTS (
     SELECT 1
       FROM t_exposure_time_mode sci
      WHERE sci.c_observation_id = acq.c_observation_id
        AND sci.c_role = 'science'::e_exposure_time_mode_role
        AND sci.c_signal_to_noise_at = acq.c_signal_to_noise_at
   );

-- The ITC input hash changes for GNIRS observations whose acquisition S/N is now
-- derived, and results cached before the two-pass classification existed carry no
-- gnirsAcqType at all.  Drop the unfrozen ones so they are recomputed; the UPDATE above
-- has already re-enqueued those observations via etm_obscalc_invalidate_trigger.
DELETE FROM t_itc_result r
 WHERE NOT r.c_is_frozen
   AND EXISTS (
     SELECT 1
       FROM t_observation o
      WHERE o.c_observation_id = r.c_observation_id
        AND o.c_observing_mode_type IN (
              'gnirs_long_slit'::e_observing_mode_type,
              'gnirs_ifu'::e_observing_mode_type,
              'gnirs_imaging'::e_observing_mode_type
            )
   );
