-- Coadds move from the GNIRS imaging mode to the individual filters, so that
-- each filter's coadds can accompany its own exposure time mode.  This mirrors
-- the GNIRS spectroscopy central wavelength configurations, where the coadds
-- live with the wavelength's exposure time mode.

ALTER TABLE t_gnirs_imaging_filter
  ADD COLUMN c_coadds int4 NOT NULL DEFAULT 1 CHECK (c_coadds > 0);

COMMENT ON COLUMN t_gnirs_imaging_filter.c_coadds IS
  'Coadds per exposure for this filter.  Always 1 for a signal-to-noise exposure time mode, which does not support coadds.';

-- Existing observations keep the mode-level value, in both row versions.
UPDATE t_gnirs_imaging_filter f
   SET c_coadds = i.c_coadds
  FROM t_gnirs_imaging i
 WHERE i.c_observation_id = f.c_observation_id;

-- ... except where the exposure time mode is signal-to-noise, which does not
-- support coadds.
UPDATE t_gnirs_imaging_filter f
   SET c_coadds = 1
  FROM t_exposure_time_mode m
 WHERE m.c_exposure_time_mode_id = f.c_exposure_time_mode_id
   AND m.c_exposure_time_mode = 'signal_to_noise';

-- v_gnirs_imaging selects i.*, so the column cannot be dropped beneath it.
DROP VIEW v_gnirs_imaging;

ALTER TABLE t_gnirs_imaging
  DROP COLUMN c_coadds;

CREATE VIEW v_gnirs_imaging AS
  SELECT
    i.*,
    -- well depth default: mirrors GnirsWellDepth.forCamera
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsWellDepth. Modify in sync.
    (CASE
      WHEN i.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'
      WHEN i.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'
    END)::e_gnirs_well_depth AS c_well_depth_default,
    f.c_filters,
    CASE WHEN i.c_variant = 'grouped'      THEN i.c_observation_id END AS c_grouped_observation_id,
    CASE WHEN i.c_variant = 'interleaved'  THEN i.c_observation_id END AS c_interleaved_observation_id,
    CASE WHEN i.c_variant = 'pre_imaging'  THEN i.c_observation_id END AS c_pre_imaging_observation_id
  FROM
    t_gnirs_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_gnirs_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);

-- Coadds are part of the per-filter ITC input, so where the backfill above forced
-- a signal-to-noise filter to 1 the observation's ITC input hash changes and its
-- cached result can never be matched again.  The stored result shape is unchanged,
-- so only GNIRS imaging observations are affected.
DELETE FROM t_itc_result r
 WHERE NOT r.c_is_frozen
   AND EXISTS (
     SELECT 1
       FROM t_observation o
      WHERE o.c_observation_id = r.c_observation_id
        AND o.c_observing_mode_type = 'gnirs_imaging'
   );
