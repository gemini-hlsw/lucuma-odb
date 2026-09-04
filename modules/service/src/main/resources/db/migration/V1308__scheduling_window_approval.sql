-- Scheduling window approval.
--
-- Science staff call an observation's timing windows its "scheduling window",
-- and by "the window" they mean the *total* time the observation is available
-- -- how long it is open, unrelated to how long it takes to execute.  The
-- requirement:
--
--   "A minimum scheduling window duration is recorded and included in the
--    accepted configurations.  Any observation using a shorter timing window
--    duration requires a CR."
--
-- So the minimum rides on the configuration request, next to the conditions,
-- target and observing mode, and is approved by the same Phase 1 review.  An
-- observation is covered by an approved configuration while its scheduling
-- window is at least the recorded minimum; a shorter one -- windows where there
-- were none, or a smaller total -- needs a change request.  Widening is free.
--
-- TARGETS OF OPPORTUNITY
--
-- A ToO cannot state absolute dates, so it states a *ToO window* instead: a
-- length, or Forever.  That stated length is its scheduling window, and the
-- trigger later opens a real window of that length starting at the request.
-- Because the length is what is recorded and compared, and it is never clipped
-- by the end of the semester, a 24 hour ToO triggered on the last night of the
-- semester is still a 24 hour ToO and stays covered.
--
-- Unstated, the ToO window defaults to 24 hours for RAPID and INTERRUPTING and
-- to Forever for STANDARD, which is exactly what the database did before this
-- migration -- so nothing changes for a PI who says nothing.

-------------------------------------------------------------------------------
-- The ToO window.
-------------------------------------------------------------------------------

CREATE FUNCTION too_default_window()
  RETURNS interval AS $$
  SELECT INTERVAL '24 hours';
$$ LANGUAGE sql IMMUTABLE;

COMMENT ON FUNCTION too_default_window() IS
  'How long a rapid or interrupting Target of Opportunity is open for once '
  'triggered, when it states no ToO window of its own.  Defined once so the '
  'window the trigger opens and the minimum the configuration records cannot drift.';

ALTER TABLE t_observation
  ADD COLUMN c_too_window         interval CHECK (c_too_window > INTERVAL '0 seconds'),
  ADD COLUMN c_too_window_forever boolean NOT NULL DEFAULT false,
  ADD CONSTRAINT too_window_exclusive
    CHECK (NOT (c_too_window IS NOT NULL AND c_too_window_forever));

COMMENT ON COLUMN t_observation.c_too_window IS
  'Stated length of the timing window this Target of Opportunity needs once '
  'triggered.  NULL with c_too_window_forever false means unstated, in which '
  'case the activation supplies the default -- see too_window_effective().';

COMMENT ON COLUMN t_observation.c_too_window_forever IS
  'True when the PI stated that this Target of Opportunity, once triggered, is '
  'open indefinitely.  Mutually exclusive with c_too_window.';

-- The ToO window in effect: the stated length, else the default for the
-- activation.  NULL means unbounded -- Forever, and the STANDARD default.
CREATE FUNCTION too_window_effective(
  activation e_too_activation,
  stated     interval,
  forever    boolean
) RETURNS interval AS $$
  SELECT CASE
    WHEN forever                                  THEN NULL
    WHEN stated IS NOT NULL                       THEN stated
    WHEN activation >= 'rapid'::e_too_activation  THEN too_default_window()
    ELSE NULL
  END;
$$ LANGUAGE sql IMMUTABLE;

-- Redefined from V1283: the window's length now comes from the observation
-- rather than being a fixed 24 hours, and a STANDARD ToO that stated a length
-- gets one too.  A ToO whose effective window is unbounded gets none, which is
-- what STANDARD did before.
CREATE OR REPLACE FUNCTION too_trigger_default_window()
  RETURNS trigger AS $$
DECLARE
  len interval;
BEGIN
  SELECT too_window_effective(NEW.c_too_activation, o.c_too_window, o.c_too_window_forever)
    INTO len
    FROM t_observation o
   WHERE o.c_observation_id = NEW.c_observation_id;

  IF len IS NOT NULL
     AND NOT EXISTS (
       SELECT 1
         FROM t_timing_window
        WHERE c_observation_id = NEW.c_observation_id
     )
  THEN
    INSERT INTO t_timing_window (
      c_observation_id,
      c_inclusion,
      c_start,
      c_end_at,
      c_automatic
    ) VALUES (
      NEW.c_observation_id,
      'include'::e_timing_window_inclusion,
      NEW.c_requested_at,
      NEW.c_requested_at + len,
      true
    );
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- v_observation expands t_observation.* at creation time, so it has to be
-- rebuilt to pick up the new columns.  Recreated verbatim from V1299.
DROP VIEW v_observation;

CREATE VIEW v_observation AS
  SELECT o.*,
  -- Grackle needs a nullable key per nullable embedded object: the ToO window
  -- object is absent when nothing was stated, and its duration is absent when
  -- what was stated is Forever.
  CASE WHEN o.c_too_window IS NOT NULL OR o.c_too_window_forever THEN o.c_observation_id END AS c_too_window_id,
  CASE WHEN o.c_too_window IS NOT NULL                           THEN o.c_observation_id END AS c_too_window_duration_id,
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

-------------------------------------------------------------------------------
-- The recorded minimum.
-------------------------------------------------------------------------------

-- Zero for rows that predate this migration: they were approved without a
-- scheduling window being part of the bargain, and zero is subsumed by any
-- observation, so nothing already approved becomes unapproved.  Every request
-- created from here on records a real value.
DROP VIEW v_configuration_request;

ALTER TABLE t_configuration_request
  ADD COLUMN c_min_scheduling_window interval NOT NULL DEFAULT INTERVAL '0 seconds'
    CHECK (c_min_scheduling_window >= INTERVAL '0 seconds');

COMMENT ON COLUMN t_configuration_request.c_min_scheduling_window IS
  'The least total time an observation may be available for scheduling and '
  'still be covered by this request.  Recorded from the observation when the '
  'request is created.  Zero means unconstrained.';

CREATE VIEW v_configuration_request AS
  SELECT
    *,
    CASE WHEN cr.c_reference_ra IS NOT NULL THEN cr.c_configuration_request_id END AS c_reference_id,
    CASE WHEN cr.c_region_ra_arc_type IS NOT NULL THEN cr.c_configuration_request_id END AS c_region_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_long_slit' THEN cr.c_configuration_request_id END AS c_flamingos_2_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_mos' THEN cr.c_configuration_request_id END AS c_flamingos_2_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_imaging' THEN cr.c_configuration_request_id END AS c_gmos_north_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_imaging' THEN cr.c_configuration_request_id END AS c_gmos_south_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_north_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_south_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_mos' THEN cr.c_configuration_request_id END AS c_gmos_north_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_mos' THEN cr.c_configuration_request_id END AS c_gmos_south_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'igrins_2_long_slit' THEN cr.c_configuration_request_id END AS c_igrins_2_longslit_id,
    CASE WHEN cr.c_gmos_north_ifu_grating IS NOT NULL AND cr.c_gmos_north_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_north_ifu_id,
    CASE WHEN cr.c_gmos_south_ifu_grating IS NOT NULL AND cr.c_gmos_south_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_south_ifu_id,
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_gnirs_ifu_grating IS NOT NULL AND cr.c_gnirs_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_ifu_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;

-- The uniqueness key must carry exactly the columns SelectRequest matches
-- exactly (see V1298), and the minimum is one of them: two requests that differ
-- only in the window they were approved for are genuinely different requests.
ALTER TABLE t_configuration_request
  DROP CONSTRAINT t_configuration_request_unique;

ALTER TABLE t_configuration_request
  ADD CONSTRAINT t_configuration_request_unique UNIQUE NULLS NOT DISTINCT (
    c_program_id,
    c_cloud_extinction,
    c_image_quality,
    c_sky_background,
    c_water_vapor,
    c_reference_ra,
    c_reference_dec,
    c_region_ra_arc_type,
    c_region_ra_arc_start,
    c_region_ra_arc_end,
    c_region_dec_arc_type,
    c_region_dec_arc_start,
    c_region_dec_arc_end,
    c_observing_mode_type,
    c_flamingos_2_longslit_disperser,
    c_gmos_north_longslit_grating,
    c_gmos_south_longslit_grating,
    c_gnirs_longslit_grating,
    c_gnirs_longslit_camera,
    c_gnirs_longslit_prism,
    c_gnirs_ifu_grating,
    c_gnirs_ifu_fpu,
    c_visitor_radius,
    c_gmos_north_ifu_grating,
    c_gmos_north_ifu_fpu,
    c_gmos_south_ifu_grating,
    c_gmos_south_ifu_fpu,
    c_min_scheduling_window
  );

-------------------------------------------------------------------------------
-- Invalidation.
--
-- Whether an observation is covered by an approved configuration is computed by
-- obscalc, and it now depends on the timing windows, the ToO window, and the
-- program's active period.  None of the three reached obscalc before.
-------------------------------------------------------------------------------

CREATE TRIGGER timing_window_obscalc_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_timing_window
  FOR EACH ROW
  EXECUTE FUNCTION obsid_obscalc_invalidate();

CREATE TRIGGER too_window_obscalc_invalidate_trigger
  AFTER UPDATE OF c_too_window, c_too_window_forever ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION obsid_obscalc_invalidate();

CREATE FUNCTION active_period_obscalc_invalidate()
  RETURNS trigger AS $$
BEGIN
  IF NEW.c_active_start <> OLD.c_active_start
     OR NEW.c_active_end <> OLD.c_active_end
  THEN
    CALL invalidate_all_obscalc_for_program(NEW.c_program_id);
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER active_period_obscalc_invalidate_trigger
  AFTER UPDATE OF c_active_start, c_active_end ON t_program
  FOR EACH ROW
  EXECUTE FUNCTION active_period_obscalc_invalidate();

COMMENT ON FUNCTION active_period_obscalc_invalidate() IS
  'Recomputes every observation in a program whose active period moved: the '
  'period bounds the scheduling window, so moving it can change which '
  'observations are covered by their approved configurations.';
