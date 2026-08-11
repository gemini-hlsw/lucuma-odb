-- Part 2 of 2 (see V1240).  Establishes the Target-of-Opportunity "aggressor"
-- axis: each observation declares its own activation level, and the proposal
-- carries the ceiling that no observation may exceed.
--
-- THE CEILING IS EXPLICIT-OR-DERIVED
--
-- c_too_activation on t_proposal goes back to nullable (reversing V0835, which
-- made it NOT NULL DEFAULT 'none' -- deliberate then, wrong now):
--
--   NULL     -> not chosen; derive the ceiling as the maximum activation among
--               the program's own observations
--   non-NULL -> chosen, by the PI before submission or by staff at TAC time
--
-- The derived form exists so a PI who simply marks observations gets a coherent
-- proposal without filling in a second field.  It is only safe *before*
-- acceptance: a live max() ceiling would be self-defeating afterwards, since
-- adding an interrupting observation would raise its own ceiling.  So on the
-- transition to 'accepted' the service materializes the effective value into
-- this column, freezing a description into an authorization.  From then on the
-- derivation is never consulted for that program again.
--
-- The derived value is capped by what the proposal type permits (see
-- too_activation_permitted below).  Without the cap, a proposal type forbidden
-- from ToO could acquire a non-none ceiling through the back door by having ToO
-- observations.  Explicit values need no cap: the t_proposal_type_checks
-- trigger already rejects them.

-------------------------------------------------------------------------------
-- Observation side.
-------------------------------------------------------------------------------

-- Adding a column with a constant default is metadata-only (no table rewrite,
-- no triggers fired), so this needs no separate migration.
ALTER TABLE t_observation
  ADD COLUMN c_too_activation e_too_activation NOT NULL DEFAULT 'none';

-- Restructure c_execution_requirement into the explicit/default/effective triple
-- the observing modes use.  The stored column becomes the *explicit* value --
-- NULL meaning "not specified" -- while the default and the effective value are
-- computed in the views.  The default depends on the ToO activation, which is
-- why this rides along with the activation work rather than standing alone.
ALTER TABLE t_observation
  ALTER COLUMN c_execution_requirement DROP DEFAULT,
  ALTER COLUMN c_execution_requirement DROP NOT NULL;

-------------------------------------------------------------------------------
-- Proposal side.
-------------------------------------------------------------------------------

ALTER TABLE t_proposal
  ALTER COLUMN c_too_activation DROP DEFAULT,
  ALTER COLUMN c_too_activation DROP NOT NULL;

-- The execution requirement an observation gets when it states none of its own.
-- A rapid or interrupting Target of Opportunity is triggered precisely because
-- the target will not wait, so letting it be interrupted would defeat the
-- purpose: its default is Uninterruptible.  Everything else defaults to
-- Unconstrained.
CREATE FUNCTION execution_requirement_default(
  activation e_too_activation
) RETURNS e_execution_requirement AS $$
  SELECT CASE WHEN activation >= 'rapid'::e_too_activation
              THEN 'uninterruptible'::e_execution_requirement
              ELSE 'unconstrained'::e_execution_requirement
         END;
$$ LANGUAGE sql IMMUTABLE;

-- What the Scheduler honors.  Note this is not the plain COALESCE(explicit,
-- default) of the observing modes: the default is a *floor*, so an explicit
-- value may tighten the requirement but never loosen it below what the ToO
-- activation demands.
CREATE FUNCTION execution_requirement_effective(
  explicit   e_execution_requirement,
  activation e_too_activation
) RETURNS e_execution_requirement AS $$
  SELECT GREATEST(
    COALESCE(explicit, 'unconstrained'::e_execution_requirement),
    execution_requirement_default(activation)
  );
$$ LANGUAGE sql IMMUTABLE;

-- The rule for which proposal types may have ToO at all, extracted from
-- t_proposal_type_checks so that the trigger and the ceiling derivation share
-- one definition instead of drifting apart.
CREATE FUNCTION too_activation_permitted(
  observatory e_observatory,
  subtype     e_science_subtype
) RETURNS boolean AS $$
  SELECT observatory = 'gemini'
     AND subtype IS NOT NULL
     AND subtype NOT IN ('classical', 'poor_weather');
$$ LANGUAGE sql IMMUTABLE;

-- The ToO ceiling a proposal gets when it states none of its own: the most
-- disruptive activation among the program's own observations, capped by what
-- the proposal type permits.  Calibration and deleted observations are excluded
-- -- they are not part of what the PI is proposing.
--
-- STABLE rather than IMMUTABLE: it reads t_observation.
CREATE FUNCTION too_activation_ceiling_default(
  pid         d_program_id,
  observatory e_observatory,
  subtype     e_science_subtype
) RETURNS e_too_activation AS $$
  SELECT LEAST(
    (
      SELECT COALESCE(MAX(o.c_too_activation), 'none'::e_too_activation)
        FROM t_observation o
       WHERE o.c_program_id = pid
         AND o.c_existence = 'present'
         AND o.c_calibration_role IS NULL
    ),
    CASE WHEN too_activation_permitted(observatory, subtype)
         THEN 'interrupting'::e_too_activation
         ELSE 'none'::e_too_activation
    END
  );
$$ LANGUAGE sql STABLE;

-- Redefined from V1214, with the TOO activation check delegating to the shared
-- function.  Behaviour is unchanged for non-NULL values; a NULL activation now
-- passes, because "not chosen" is not a violation -- the derivation caps it.
-- The ch_proposal_type trigger references this function by name, so it does not
-- need to be recreated.
CREATE OR REPLACE FUNCTION t_proposal_type_checks()
RETURNS TRIGGER AS $$
DECLARE
  is_gemini boolean           := (NEW.c_observatory = 'gemini');
  st        e_science_subtype := NEW.c_science_subtype;
BEGIN
  -- A science subtype is present iff the proposal is a Gemini proposal.
  IF is_gemini AND st IS NULL THEN
    RAISE EXCEPTION 'Gemini proposals must define a science subtype.';
  END IF;
  IF (NOT is_gemini) AND st IS NOT NULL THEN
    RAISE EXCEPTION 'Only Gemini proposals may define a science subtype.';
  END IF;

  -- An exchange partner may only be set on Gemini queue or classical proposals.
  IF NEW.c_exchange_partner IS NOT NULL AND NOT (is_gemini AND st IN ('queue', 'classical')) THEN
    RAISE EXCEPTION 'An exchange partner may only be set on Gemini queue or classical proposals.';
  END IF;

  -- TOO activation, when explicitly chosen, must be None except for Gemini
  -- subtypes other than classical and poor weather.
  IF NEW.c_too_activation IS NOT NULL
       AND NEW.c_too_activation <> 'none'
       AND NOT too_activation_permitted(NEW.c_observatory, st) THEN
    RAISE EXCEPTION 'TOO activation must be None for this proposal type.';
  END IF;

  -- Minimum percent time must be 0 for Gemini poor weather proposals.
  IF NEW.c_min_percent <> 0 AND is_gemini AND st = 'poor_weather' THEN
    RAISE EXCEPTION 'Minimum percent time must be 0 for Gemini poor weather proposals.';
  END IF;

  -- Total time and min percent total are set if and only if the proposal is a
  -- Gemini large program.
  IF (NEW.c_total_time IS NOT NULL OR NEW.c_min_percent_total IS NOT NULL)
       AND NOT (is_gemini AND st = 'large_program') THEN
    RAISE EXCEPTION 'Total time and min percent total may only be set on Gemini large programs.';
  END IF;
  IF is_gemini AND st = 'large_program'
       AND (NEW.c_total_time IS NULL OR NEW.c_min_percent_total IS NULL) THEN
    RAISE EXCEPTION 'Large Program proposals must define the total time and min percent total.';
  END IF;

  -- US long term may only be set on Gemini classical or queue proposals.
  IF NEW.c_us_long_term AND NOT (is_gemini AND st IN ('classical', 'queue')) THEN
    RAISE EXCEPTION 'US long term may only be set on Gemini classical or queue proposals.';
  END IF;

  -- AEON multi-facility and JWST synergy may only be set on Gemini classical,
  -- large program, or queue proposals.
  IF (NEW.c_aeon_multi_facility OR NEW.c_jwst_synergy)
       AND NOT (is_gemini AND st IN ('classical', 'large_program', 'queue')) THEN
    RAISE EXCEPTION 'AEON multi-facility and JWST synergy may only be set on Gemini classical, large program, or queue proposals.';
  END IF;

  -- Consider for band 3 may only be set on Gemini queue proposals.
  IF NEW.c_consider_for_band_3 <> 'unset' AND NOT (is_gemini AND st = 'queue') THEN
    RAISE EXCEPTION 'Consider for band 3 may only be set on Gemini queue proposals.';
  END IF;

  -- A mentor may only be set on Gemini fast turnaround proposals.
  IF NEW.c_mentor_id IS NOT NULL AND NOT (is_gemini AND st = 'fast_turnaround') THEN
    RAISE EXCEPTION 'A mentor may only be set on Gemini fast turnaround proposals.';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------------
-- Views.
-------------------------------------------------------------------------------

-- v_observation is `SELECT o.*`, which expands its column list at creation time
-- and so does not pick up c_too_activation on its own.  Body otherwise copied
-- verbatim from V1239, plus the effective execution requirement.
DROP VIEW v_generator_params;
DROP VIEW v_observation;

CREATE VIEW v_observation AS
  SELECT o.*,
  -- Explicit / default / effective, as for the observing modes.  The explicit
  -- value is o.c_execution_requirement itself (nullable); the ToO activation
  -- decides the default, and the default acts as a floor beneath the effective
  -- value.  The explicit value is stored untouched, so lowering the activation
  -- restores it.
  execution_requirement_default(o.c_too_activation) AS c_execution_requirement_default,
  execution_requirement_effective(o.c_execution_requirement, o.c_too_activation)
    AS c_execution_requirement_effective,
  -- The deprecated flag reflects what the Scheduler actually honors.
  (execution_requirement_effective(o.c_execution_requirement, o.c_too_activation)
     = 'unconstrained'::e_execution_requirement) AS c_is_splittable,
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

-- v_generator_params as in V1239, except that it now carries the *effective*
-- execution requirement.  The generator asks whether the sequence may be split;
-- for a rapid or interrupting ToO the answer is no regardless of what was
-- declared, so it must see the floored value.
CREATE VIEW v_generator_params AS
SELECT
  o.c_program_id,
  o.c_observation_id,
  o.c_calibration_role,
  o.c_image_quality,
  o.c_cloud_extinction,
  o.c_sky_background,
  o.c_water_vapor,
  o.c_air_mass_min,
  o.c_air_mass_max,
  o.c_hour_angle_min,
  o.c_hour_angle_max,
  e.c_exposure_time_mode,
  e.c_signal_to_noise,
  e.c_signal_to_noise_at,
  e.c_exposure_time,
  e.c_exposure_count,
  o.c_observing_mode_type,
  o.c_science_band,
  o.c_declared_state,
  CASE
    -- The observation has a declared state.
    WHEN o.c_declared_state IS NOT NULL THEN o.c_declared_state

    -- No events have been fired at all -> not_started (just slewing to the
    -- target doesn't count as execution).
    WHEN NOT EXISTS (
      SELECT 1
      FROM   t_execution_event v
      WHERE  v.c_observation_id = o.c_observation_id
        AND  v.c_event_type != 'slew'::e_execution_event_type
    ) THEN 'not_started'::e_execution_state

    -- At least one step not completed -> ongoing
    WHEN EXISTS (
      SELECT 1
      FROM t_step s
      JOIN t_atom a ON a.c_atom_id = s.c_atom_id AND a.c_observation_id = o.c_observation_id AND a.c_sequence_type = 'science'
      LEFT JOIN t_step_execution se       ON se.c_step_id = s.c_step_id
      LEFT JOIN t_step_execution_state es ON es.c_tag     = se.c_execution_state AND es.c_terminal
      WHERE es.c_tag IS NULL -- no step execution or a non-terminal execution state
    ) THEN 'ongoing'::e_execution_state

    ELSE 'completed'::e_execution_state
  END AS c_execution_state,
  COALESCE(s_counts.c_step_count, 0) AS c_step_count,
  execution_requirement_effective(o.c_execution_requirement, o.c_too_activation)
    AS c_execution_requirement,
  o.c_blind_offset_target_id,
  b.c_sid_rv AS c_blind_rv,
  b.c_source_profile AS c_blind_source_profile,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile,
  COALESCE(t.c_is_signal_to_noise_target, false) AS c_is_signal_to_noise_target
FROM
  t_observation o
LEFT JOIN t_target b ON b.c_target_id = o.c_blind_offset_target_id
LEFT JOIN LATERAL (
  SELECT t.c_target_id,
         t.c_sid_rv,
         t.c_source_profile,
         a.c_is_signal_to_noise_target
    FROM t_asterism_target a
    INNER JOIN t_target t
      ON a.c_target_id = t.c_target_id
     AND t.c_existence = 'present'
   WHERE a.c_observation_id = o.c_observation_id
) t ON TRUE
LEFT JOIN t_exposure_time_mode e
  ON e.c_observation_id = o.c_observation_id
 AND e.c_role = 'requirement'
LEFT JOIN (
  SELECT
    se.c_observation_id,
    COUNT(*) AS c_step_count
  FROM t_step_execution se
  GROUP BY se.c_observation_id
) s_counts ON s_counts.c_observation_id = o.c_observation_id
ORDER BY
  o.c_observation_id,
  t.c_target_id;

-- v_proposal as in V1214, plus the effective ToO ceiling.  Calibration
-- observations and deleted observations are excluded from the derivation: they
-- are not part of what the PI is proposing.
DROP VIEW v_proposal;

CREATE VIEW v_proposal AS
  SELECT
    p.*,
    -- Explicit / default / effective.  Unlike the observation's execution
    -- requirement, this is a plain COALESCE: an explicit ceiling replaces the
    -- derived one outright rather than acting as a floor beneath it.  Note the
    -- default keeps tracking the observations even after acceptance, when it no
    -- longer has any effect -- see the freeze in ProposalService.
    too_activation_ceiling_default(p.c_program_id, p.c_observatory, p.c_science_subtype)
                                                                             AS c_too_activation_default,
    COALESCE(
      p.c_too_activation,
      too_activation_ceiling_default(p.c_program_id, p.c_observatory, p.c_science_subtype)
    )                                                                        AS c_too_activation_effective,
    CASE WHEN p.c_observatory = 'gemini'               THEN c_program_id END AS c_program_id_gemini,
    CASE WHEN p.c_observatory = 'keck'                 THEN c_program_id END AS c_program_id_keck,
    CASE WHEN p.c_observatory = 'subaru'               THEN c_program_id END AS c_program_id_subaru,
    -- Non-null discriminator for the GeminiProposalType interface mapping.  Only
    -- meaningful for Gemini proposals; others get a placeholder that is never
    -- rendered (their c_program_id_gemini key is null).
    COALESCE(p.c_science_subtype, 'queue')                                   AS c_gemini_science_subtype,
    CASE WHEN p.c_science_subtype = 'classical'           THEN c_program_id END AS c_program_id_c,
    CASE WHEN p.c_science_subtype = 'demo_science'        THEN c_program_id END AS c_program_id_s,
    CASE WHEN p.c_science_subtype = 'directors_time'      THEN c_program_id END AS c_program_id_d,
    CASE WHEN p.c_science_subtype = 'fast_turnaround'     THEN c_program_id END AS c_program_id_f,
    CASE WHEN p.c_science_subtype = 'large_program'       THEN c_program_id END AS c_program_id_l,
    CASE WHEN p.c_science_subtype = 'poor_weather'        THEN c_program_id END AS c_program_id_p,
    CASE WHEN p.c_science_subtype = 'queue'               THEN c_program_id END AS c_program_id_q,
    CASE WHEN p.c_science_subtype = 'system_verification' THEN c_program_id END AS c_program_id_v
  FROM
    t_proposal p;

-------------------------------------------------------------------------------
-- Backfill, last.
-------------------------------------------------------------------------------

-- Existing rows all hold the old 'none' default, and there is no way to tell an
-- explicit 'none' from a defaulted one -- so treat them all as "not chosen" and
-- let the derivation take over.  This UPDATE comes last, after every ALTER: it
-- queues deferred trigger events on t_proposal, and PostgreSQL refuses to ALTER
-- a table with pending trigger events in the same transaction.
UPDATE t_proposal
   SET c_too_activation = NULL
 WHERE c_too_activation = 'none';

-- Likewise for the execution requirement: every existing row holds the old
-- 'unconstrained' default and there is no way to tell an explicit choice from a
-- defaulted one, so treat them all as unspecified.  Effective values are
-- unchanged, since the default is 'unconstrained'.
UPDATE t_observation
   SET c_execution_requirement = NULL
 WHERE c_execution_requirement = 'unconstrained';
