-- Altair (Gemini North adaptive optics) configuration.

-- Tags match lucuma.core.enums.AltairMode
-- Enum types mirror lucuma.core.enums.AltairMode, FieldLens, AltairNdFilter and CassRotator.
CREATE TYPE e_altair_mode      AS ENUM ('ngs', 'lgs', 'lgs_p1');
CREATE TYPE e_field_lens       AS ENUM ('in', 'out');
CREATE TYPE e_altair_nd_filter AS ENUM ('in', 'out');
CREATE TYPE e_cass_rotator     AS ENUM ('fixed', 'following');

-- Altair holds its natural guide star on its own wavefront sensor.
INSERT INTO t_guide_probe VALUES ('AltairAowfs', 'Altair AOWFS', 'Altair adaptive optics WFS');

ALTER TABLE t_observation
  ADD COLUMN c_altair_mode         e_altair_mode      NULL,
  -- null means the field lens is chosen automatically from the guide star separation
  ADD COLUMN c_altair_field_lens   e_field_lens       NULL,
  ADD COLUMN c_altair_cass_rotator e_cass_rotator     NULL,
  ADD COLUMN c_altair_nd_filter    e_altair_nd_filter NULL;

-- Single-row invariants, so a CHECK is safe here (see the contributor guide).
ALTER TABLE t_observation
  ADD CONSTRAINT altair_all_or_nothing CHECK (
    (c_altair_mode IS NULL AND c_altair_field_lens IS NULL AND c_altair_cass_rotator IS NULL AND c_altair_nd_filter IS NULL)
    OR
    (c_altair_mode IS NOT NULL AND c_altair_cass_rotator IS NOT NULL AND c_altair_nd_filter IS NOT NULL)
  ),
  -- LGS modes always use the field lens.
  ADD CONSTRAINT altair_lgs_field_lens CHECK (
    c_altair_mode = 'ngs' OR c_altair_field_lens IS NULL OR c_altair_field_lens = 'in'
  ),
  -- The ND filter is not commissioned for the LGS modes.
  ADD CONSTRAINT altair_lgs_nd_filter CHECK (
    c_altair_mode = 'ngs' OR c_altair_nd_filter IS NULL OR c_altair_nd_filter = 'out'
  );

-- v_observation selects o.*, so it must be recreated to pick up the new columns.
DROP VIEW v_observation;

-- Body copied verbatim from V1313, plus the synthetic Altair key.
CREATE VIEW v_observation AS
  SELECT o.*,
  (o.c_scheduling_mode = 'unconstrained'::e_scheduling_mode) AS c_is_splittable,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,
  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  CASE WHEN o.c_orig_est_setup_count     IS NOT NULL THEN o.c_observation_id END AS c_original_estimate_id,
  CASE WHEN o.c_altair_mode              IS NOT NULL THEN o.c_observation_id END AS c_altair_id,
  -- Alias read as a plain nullable column; c_altair_cass_rotator itself is mapped
  -- as a non-null field of the nested Altair object and a grackle ColumnRef is
  -- identified by name alone, so the two uses need distinct names.
  o.c_altair_cass_rotator AS c_cass_rotator,
  CASE WHEN o.c_science_mode = 'imaging'::d_tag      THEN o.c_observation_id END AS c_imaging_mode_id,
  CASE WHEN o.c_science_mode = 'spectroscopy'::d_tag THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time,
  EXISTS (
    SELECT 1
    FROM t_sequence_materialization m
    WHERE m.c_observation_id = o.c_observation_id
      AND m.c_sequence_type = 'science'::e_sequence_type
  ) AS c_science_sequence_is_materialized,
  EXISTS (
    SELECT 1
    FROM t_sequence_materialization m
    WHERE m.c_observation_id = o.c_observation_id
      AND m.c_sequence_type = 'acquisition'::e_sequence_type
  ) AS c_acquisition_sequence_is_materialized,
  (
    SELECT a.c_target_id
    FROM t_asterism_target a
    WHERE a.c_observation_id = o.c_observation_id
      AND a.c_is_signal_to_noise_target
  ) AS c_signal_to_noise_target_id
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;

-- Which instruments can observe behind Altair. IGRINS-2 is expected to join
-- GNIRS here.
ALTER TABLE t_instrument
  ADD COLUMN c_altair boolean NOT NULL DEFAULT false;

COMMENT ON COLUMN t_instrument.c_altair IS
  'Whether the instrument can observe behind the Altair adaptive optics system.';

UPDATE t_instrument SET c_altair = true WHERE c_tag = 'Gnirs';

-- The instrument flag lives in another table, so this invariant is a deferrable
-- constraint trigger rather than a CHECK (see the contributor guide).
CREATE FUNCTION check_altair_instrument()
RETURNS TRIGGER AS $$
BEGIN

  -- A null instrument means the observation has no observing mode yet, which is
  -- allowed; the check runs again when the mode arrives.
  IF NEW.c_altair_mode IS NOT NULL AND NEW.c_instrument IS NOT NULL AND NOT EXISTS (
    SELECT 1
      FROM t_instrument
     WHERE c_tag = NEW.c_instrument
       AND c_altair
  ) THEN
    RAISE EXCEPTION 'Altair is not available for instrument %', NEW.c_instrument;
  END IF;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_observation_altair_instrument
  AFTER INSERT OR UPDATE OF c_altair_mode, c_instrument ON t_observation
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_altair_instrument();
