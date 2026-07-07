-- GNIRS Imaging observing mode, modeled after Flamingos2 imaging.
-- Keyhole imaging fixes the FPU (acquisition keyhole), the decker (acquisition)
-- and the acquisition mirror (in), so none of them is stored.
CREATE TABLE t_gnirs_imaging (

  c_observation_id      d_observation_id      NOT NULL,
  c_program_id          d_program_id          NOT NULL,
  c_instrument          d_tag                 NOT NULL DEFAULT 'Gnirs' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'Gnirs'),
  c_observing_mode_type e_observing_mode_type NOT NULL DEFAULT 'gnirs_imaging' CHECK (c_observing_mode_type = 'gnirs_imaging'),

  c_camera              d_tag                 NOT NULL REFERENCES t_gnirs_camera(c_tag),
  c_coadds              int4                  NOT NULL DEFAULT 1 CHECK (c_coadds > 0),

  -- Explicit overrides: a NULL read mode is derived per step from the exposure
  -- time; a NULL well depth is derived from the camera.
  c_read_mode           d_tag                 NULL DEFAULT NULL,
  c_well_depth          e_gnirs_well_depth    NULL DEFAULT NULL,

  -- Imaging variant columns, shared shape with the f2/gmos imaging mode tables.
  c_variant             e_imaging_variant     NOT NULL DEFAULT 'grouped',
  c_wavelength_order    e_wavelength_order    NOT NULL DEFAULT 'increasing',
  c_sky_count           int                   NOT NULL DEFAULT 0 CHECK (c_sky_count >= 0),
  c_pre_imaging_off1_p  d_angle_µas           NOT NULL DEFAULT 0,
  c_pre_imaging_off1_q  d_angle_µas           NOT NULL DEFAULT 0,
  c_pre_imaging_off2_p  d_angle_µas           NOT NULL DEFAULT 0,
  c_pre_imaging_off2_q  d_angle_µas           NOT NULL DEFAULT 0,
  c_pre_imaging_off3_p  d_angle_µas           NOT NULL DEFAULT 0,
  c_pre_imaging_off3_q  d_angle_µas           NOT NULL DEFAULT 0,
  c_pre_imaging_off4_p  d_angle_µas           NOT NULL DEFAULT 0,
  c_pre_imaging_off4_q  d_angle_µas           NOT NULL DEFAULT 0,

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  UNIQUE (c_observation_id),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

COMMENT ON TABLE t_gnirs_imaging IS 'GNIRS Imaging mode configuration';

-- Table for the list of filters used in a GNIRS imaging observation with an
-- associated etm.  Modeled after the Flamingos2 imaging filter table.
CREATE TABLE t_gnirs_imaging_filter (
  c_observation_id        d_observation_id             NOT NULL,
  c_filter                d_tag                        NOT NULL REFERENCES t_gnirs_filter(c_tag),
  c_version               e_observing_mode_row_version NOT NULL DEFAULT 'current',
  c_exposure_time_mode_id integer                      NOT NULL,
  c_role                  e_exposure_time_mode_role    NOT NULL DEFAULT 'science' CHECK (c_role = 'science'),

  PRIMARY KEY (c_observation_id, c_filter, c_version),
  CONSTRAINT t_gnirs_imaging_filter_unique_exposure_time_mode_id
    UNIQUE (c_exposure_time_mode_id),
  FOREIGN KEY (c_observation_id)
    REFERENCES t_gnirs_imaging(c_observation_id) ON DELETE CASCADE,
  FOREIGN KEY (c_exposure_time_mode_id, c_role)
    REFERENCES t_exposure_time_mode(c_exposure_time_mode_id, c_role)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

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

-- Register the mode for the observing mode consistency trigger.
SELECT register_observing_mode('gnirs_imaging', 't_gnirs_imaging');

-- Update check_etm_consistent: like the other imaging modes, gnirs_imaging has
-- per-filter science etms and no acquisition etm, so there is nothing to check.
CREATE OR REPLACE FUNCTION check_etm_consistent()
RETURNS TRIGGER AS $$
DECLARE
  obs_id   d_observation_id;
  obs_mode e_observing_mode_type;
  acq_count INTEGER;
  sci_count INTEGER;
BEGIN

  obs_id := COALESCE(NEW.c_observation_id, OLD.c_observation_id);

  SELECT c_observing_mode_type INTO obs_mode
    FROM t_observation
   WHERE c_observation_id = obs_id;

  SELECT
    COUNT(*) FILTER (WHERE c_role = 'acquisition'),
    COUNT(*) FILTER (WHERE c_role = 'science')
  INTO acq_count, sci_count
  FROM t_exposure_time_mode
  WHERE c_observation_id = obs_id;

  IF obs_mode IS NULL THEN

    IF acq_count <> 0 OR sci_count <> 0 THEN
      RAISE EXCEPTION 'Observation % with mode % should not have acquisition nor science exposure time modes', obs_id, obs_mode;
    END IF;

  ELSE

    CASE
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit', 'gnirs_long_slit', 'gnirs_ifu') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'ghost_ifu' THEN
        IF sci_count <> 2 THEN
          RAISE EXCEPTION 'Observation % with mode % must have two science exposure time modes (red and blue camera)', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'igrins_2_long_slit' THEN
        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging', 'flamingos_2_imaging', 'gnirs_imaging') THEN
        NULL;

      -- no checks for visitor modes
      WHEN obs_mode IN (
        'alopeke_speckle',
        'alopeke_wide_field',
        'visitor_north',
        'visitor_south',
        'zorro_speckle',
        'zorro_wide_field',
        'maroon_x'
      ) THEN
        NULL;

      WHEN obs_mode IN ('exchange_keck', 'exchange_subaru') THEN
        IF acq_count <> 0 OR sci_count <> 0 THEN
          RAISE EXCEPTION 'Observation % with mode % should not have acquisition nor science exposure time modes', obs_id, obs_mode;
        END IF;

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Obs events and obs calc triggers
CREATE TRIGGER ch_observation_edit_gnirs_imaging_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_gnirs_imaging
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

-- Clean up offset generators when the imaging mode row is deleted.
CREATE TRIGGER offset_generation_cleanup_trigger
AFTER DELETE ON t_gnirs_imaging
FOR EACH ROW
EXECUTE FUNCTION offset_generation_cleanup();
