-- Part 1 of 3 (see V1326, V1327).  The Target of Opportunity redesign: what an
-- observation may do to others, what may be done to it, and how both are
-- approved.
--
-- V1261 made the activation a function of the asterism and the scheduling mode.
-- That tied "is this a Target of Opportunity" to holding an opportunity target,
-- which in turn required the target to survive the alert -- to be resolved in
-- place rather than replaced.  Science staff rejected both halves: resolution
-- means two code paths to a target's coordinates, and an observation that
-- silently becomes a ToO because of what its asterism holds is too easy to get
-- wrong at the moment it matters.
--
-- So the two axes separate completely:
--
--   c_scheduling_mode  what the Scheduler may do TO this observation
--   c_too_activation   what this observation may do TO OTHERS
--
-- Neither derives from the other and neither derives from the asterism.  What
-- remains between them is a single compatibility rule, stated as a CHECK below:
-- a rapid or interrupting ToO must itself be uninterruptible.
--
-- Each axis also loses a value.  'interrupting' moves off the mode and onto the
-- activation, where it already had a twin, leaving the mode the three rungs that
-- are genuinely about being a victim.  'standard' goes from the activation: a ToO
-- content to be observed whenever convenient once its event happens is scheduled
-- exactly like ordinary science, and without a trigger record it has nothing but
-- a label to distinguish it from 'none'.  A label with no consequence gets
-- applied inconsistently, so every 'standard' becomes 'none'.
--
-- The two axes are approved in different places.  The mode is part of an
-- observation's configuration, so configuration requests gain it and match it by
-- subsumption: an approved mode covers itself and every looser one.  The
-- activation is approved program-wide, as a ceiling that moves from the proposal
-- to the program (t_program.c_too_activation_ceiling).  A null ceiling means no
-- restriction; a set one is enforced, whatever kind of program it is on.
-- Acceptance fills in a null with the proposal type's default -- 'none' for types
-- that ordinarily may not have Targets of Opportunity, and otherwise the highest
-- activation among the program's observations -- and staff may set or clear it on
-- any program.
--
-- The opportunity target does not go away.  It is a placeholder carrying an
-- approved region, swapped out for a real target when the alert arrives. V1326
-- and V1327 carry that half out on t_target.
--
-- The rewrites of the two enums are pure DDL, remapping inside USING, so none of
-- t_observation's row triggers fire.  The two backfills, of the program ceiling
-- and of the configuration requests' mode, each follow every ALTER of their
-- table and run with user triggers disabled.

-------------------------------------------------------------------------------
-- Dependents come down first.
-------------------------------------------------------------------------------

-- Everything that binds either enum or names c_too_activation comes down here
-- and goes back up below, once, in its final form.
DROP TRIGGER too_trigger_track_ready_trigger ON t_observation;
DROP TRIGGER too_trigger_ceiling_withdraw_trigger ON t_proposal;
DROP VIEW v_generator_params;
DROP VIEW v_observation;
DROP VIEW v_proposal;
DROP FUNCTION too_activation_ceiling_default(d_program_id, e_observatory, e_science_subtype);
ALTER TABLE t_too_trigger DROP CONSTRAINT too_trigger_activation_not_none;

-------------------------------------------------------------------------------
-- The activation becomes declared.
-------------------------------------------------------------------------------

-- DROP EXPRESSION converts a stored generated column into an ordinary one and
-- keeps every value it had computed. The values it freezes are the ones V1261
-- derived, which is the correct starting point.
ALTER TABLE t_observation
  ALTER COLUMN c_too_activation DROP EXPRESSION;

-- Nothing derives the activation any more.
DROP FUNCTION too_activation(boolean, e_scheduling_mode);

-------------------------------------------------------------------------------
-- The scheduling mode loses 'interrupting'.
-------------------------------------------------------------------------------

-- PostgreSQL cannot remove a value from an enum, so the type is rebuilt.
-- 'interrupting' maps to 'uninterruptible'.
ALTER TYPE e_scheduling_mode RENAME TO e_scheduling_mode_old;

CREATE TYPE e_scheduling_mode AS ENUM (
  'unconstrained',
  'no_splitting',
  'uninterruptible'
);

ALTER TABLE t_observation
  ALTER COLUMN c_scheduling_mode DROP DEFAULT,
  ALTER COLUMN c_scheduling_mode TYPE e_scheduling_mode
    USING (
      CASE WHEN c_scheduling_mode::text = 'interrupting'
           THEN 'uninterruptible'
           ELSE c_scheduling_mode::text
      END::e_scheduling_mode
    ),
  ALTER COLUMN c_scheduling_mode SET DEFAULT 'unconstrained'::e_scheduling_mode;

DROP TYPE e_scheduling_mode_old;

-------------------------------------------------------------------------------
-- The activation loses 'standard'.
-------------------------------------------------------------------------------

-- A trigger at 'standard' records a declaration that no longer means anything.
UPDATE t_too_trigger
   SET c_supersedes = NULL
 WHERE c_supersedes IN (
   SELECT c_too_trigger_id
     FROM t_too_trigger
    WHERE c_too_activation = 'standard'::e_too_activation
 );

DELETE FROM t_chron_too_trigger_update
 WHERE c_too_trigger_id IN (
   SELECT c_too_trigger_id
     FROM t_too_trigger
    WHERE c_too_activation = 'standard'::e_too_activation
 );

DELETE FROM t_too_trigger
 WHERE c_too_activation = 'standard'::e_too_activation;

-- The type is rebuilt as the mode's was above.
ALTER TYPE e_too_activation RENAME TO e_too_activation_old;

CREATE TYPE e_too_activation AS ENUM (
  'none',
  'rapid',
  'interrupting'
);

ALTER TABLE t_observation
  ALTER COLUMN c_too_activation DROP DEFAULT,
  ALTER COLUMN c_too_activation TYPE e_too_activation
    USING (
      CASE WHEN c_too_activation::text = 'standard'
           THEN 'none'
           ELSE c_too_activation::text
      END::e_too_activation
    ),
  ALTER COLUMN c_too_activation SET DEFAULT 'none'::e_too_activation;

-- The proposal's ceiling.  Converted rather than dropped here because the program
-- backfill below carries it over to t_program; it is dropped after that.  A
-- program that asked for no more than 'standard' asked for nothing a ToO needs.
ALTER TABLE t_proposal
  ALTER COLUMN c_too_activation TYPE e_too_activation
    USING (
      CASE WHEN c_too_activation::text = 'standard'
           THEN 'none'
           ELSE c_too_activation::text
      END::e_too_activation
    );

-- No 'standard' remains in either: the rows holding it were deleted above.
ALTER TABLE t_too_trigger
  ALTER COLUMN c_too_activation TYPE e_too_activation
    USING c_too_activation::text::e_too_activation;

ALTER TABLE t_chron_too_trigger_update
  ALTER COLUMN c_new_too_activation TYPE e_too_activation
    USING c_new_too_activation::text::e_too_activation;

DROP TYPE e_too_activation_old;

-------------------------------------------------------------------------------
-- Constraints.
-------------------------------------------------------------------------------

-- Every trigger is for a ToO, so this column cannot take 'none'.
ALTER TABLE t_too_trigger
  ADD CONSTRAINT too_trigger_activation_not_none
    CHECK (c_too_activation <> 'none'::e_too_activation);

-- The one rule relating the two axes.  An observation that displaces other
-- science must not itself be displaceable, and one promised as soon as possible
-- should not be broken up once it starts.
ALTER TABLE t_observation
  ADD CONSTRAINT too_activation_scheduling_mode_compatible
  CHECK (
    c_too_activation = 'none'::e_too_activation
    OR c_scheduling_mode = 'uninterruptible'::e_scheduling_mode
  );

-------------------------------------------------------------------------------
-- Resolvedness stops being a question.
-------------------------------------------------------------------------------

-- A placeholder is swapped, not resolved, so "holds an opportunity target" and
-- "is still waiting" are now the same question and one flag answers both.
-- c_has_too_target stays: it is no longer an input to the activation, but it is
-- still what tells the workflow that an observation has nowhere to point.
--
-- Replaced before the column it reads is dropped.
CREATE OR REPLACE FUNCTION refresh_has_too_target(oid d_observation_id) RETURNS void AS $$
  UPDATE t_observation o
     SET c_has_too_target = h.has_too_target
    FROM (SELECT observation_has_too_target(oid) AS has_too_target) h
   WHERE o.c_observation_id = oid
     AND o.c_has_too_target IS DISTINCT FROM h.has_too_target;
$$ LANGUAGE sql;

ALTER TABLE t_observation
  DROP COLUMN c_has_unresolved_too_target;

DROP FUNCTION observation_has_unresolved_too_target(d_observation_id);

-------------------------------------------------------------------------------
-- The trigger predicate.
-------------------------------------------------------------------------------

-- Ready is still the request.  What changes is the third clause of each
-- predicate: what disqualifies an observation is holding a placeholder at all --
-- there is nowhere to point, so there is nothing to ask an observer to do.  The
-- activation is read as declared rather than derived, which the column now is.
--
-- Body otherwise copied verbatim from V1277, including the supersession arm: a
-- change of activation on a live request still closes it out and opens a
-- successor, because triggering at one activation and another are different
-- requests.
CREATE OR REPLACE FUNCTION too_trigger_track_ready()
  RETURNS trigger AS $$
DECLARE
  was_triggered bool := TG_OP = 'UPDATE'
                    AND OLD.c_workflow_user_state IS NOT DISTINCT FROM 'ready'::e_workflow_user_state
                    AND OLD.c_too_activation <> 'none'::e_too_activation
                    AND NOT OLD.c_has_too_target;
  is_triggered  bool := NEW.c_workflow_user_state IS NOT DISTINCT FROM 'ready'::e_workflow_user_state
                    AND NEW.c_too_activation <> 'none'::e_too_activation
                    AND NOT NEW.c_has_too_target;

  -- Guarded on TG_OP exactly as was_triggered is, so OLD is only read on UPDATE.
  activation_changed bool := TG_OP = 'UPDATE'
                         AND NEW.c_too_activation IS DISTINCT FROM OLD.c_too_activation;

  superseded_id d_too_trigger_id;
BEGIN
  -- A new trigger.
  IF is_triggered AND NOT was_triggered THEN
    INSERT INTO t_too_trigger (c_observation_id, c_program_id, c_too_activation)
    VALUES (NEW.c_observation_id, NEW.c_program_id, NEW.c_too_activation)
    ON CONFLICT DO NOTHING;

  -- A trigger withdrawal
  ELSIF was_triggered AND NOT is_triggered THEN
    UPDATE t_too_trigger
       SET c_status = 'withdrawn'
     WHERE c_observation_id = NEW.c_observation_id
       AND c_status = 'requested';

  -- Update the ToO activation by superseding the existing request.
  ELSIF was_triggered AND is_triggered AND activation_changed THEN
    UPDATE t_too_trigger
       SET c_status = 'superseded'
     WHERE c_observation_id = NEW.c_observation_id
       AND c_status = 'requested'
    RETURNING c_too_trigger_id INTO superseded_id;

    IF superseded_id IS NOT NULL THEN
      INSERT INTO t_too_trigger (c_observation_id, c_program_id, c_too_activation, c_supersedes)
      VALUES (NEW.c_observation_id, NEW.c_program_id, NEW.c_too_activation, superseded_id)
      ON CONFLICT DO NOTHING;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- c_too_activation is an ordinary column again, so it may appear in UPDATE OF and
-- be watched directly.  c_scheduling_mode drops out: it no longer decides
-- anything the predicate reads.
CREATE TRIGGER too_trigger_track_ready_trigger
  AFTER INSERT OR UPDATE OF c_workflow_user_state, c_too_activation, c_has_too_target ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION too_trigger_track_ready();

-------------------------------------------------------------------------------
-- Configuration requests carry the scheduling mode.
-------------------------------------------------------------------------------

-- It is part of the configuration and so part of a request's identity: two
-- observations alike in everything but their mode are asking for different
-- things.  Existing requests are backfilled at the end.
ALTER TABLE t_configuration_request
  ADD COLUMN c_scheduling_mode e_scheduling_mode NOT NULL DEFAULT 'unconstrained'::e_scheduling_mode;

-- The key must match SelectRequest's matched columns exactly (see V1298).  Every
-- existing request reads 'unconstrained' until the backfill, which then gives
-- every request in a program the same mode, so rows unique before are unique
-- still.
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
    c_altair_mode,
    c_scheduling_mode
  );

-- v_configuration_request selects *, so it is recreated to pick up the new
-- column.  Body copied verbatim from V1323.
DROP VIEW v_configuration_request;

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

-------------------------------------------------------------------------------
-- The program's ceiling.
-------------------------------------------------------------------------------

-- Body copied from V1245, as too_activation_ceiling_default.  It is the default
-- acceptance gives a program's ceiling when none has been set.
CREATE FUNCTION too_activation_max(
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

COMMENT ON FUNCTION too_activation_max(d_program_id, e_observatory, e_science_subtype) IS
  'The highest ToO activation among the program''s present, non-calibration '
  'observations, capped by what the proposal type ordinarily permits.  The '
  'ceiling acceptance gives a program that has none.';

ALTER TABLE t_program
  ADD COLUMN c_too_activation_ceiling e_too_activation NULL;

-- Accepted programs keep what they had: the proposal's explicit or frozen ceiling,
-- else the default.  Every other program starts unrestricted, which drops any
-- ceiling a PI chose before acceptance, since PIs no longer choose one.
--
-- User triggers are disabled, as V1267 did: no ceiling moves out from under a
-- live request, and nothing checked against it changes, so there is nothing to
-- withdraw and no workflow to recalculate.
ALTER TABLE t_program DISABLE TRIGGER USER;

UPDATE t_program p
   SET c_too_activation_ceiling = COALESCE(
         x.c_too_activation,
         too_activation_max(x.c_program_id, x.c_observatory, x.c_science_subtype)
       )
  FROM t_proposal x
 WHERE x.c_program_id = p.c_program_id
   AND p.c_proposal_status = 'accepted';

ALTER TABLE t_program ENABLE TRIGGER USER;

-- The proposal's column goes, and with it the proposal type check on it.  A type
-- that ordinarily may not have Targets of Opportunity only defaults to 'none'
-- now; staff may grant it more.  Body copied from V1245, less that check.
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

ALTER TABLE t_proposal
  DROP COLUMN c_too_activation;

-- Body copied from V1287, less the explicit / default / effective ceiling
-- columns.
CREATE VIEW v_proposal AS
  SELECT
    p.*,
    COALESCE(
      (SELECT ARRAY_AGG(r.c_instrument ORDER BY r.c_instrument)
         FROM t_proposal_aeon_required_instrument r
        WHERE r.c_program_id = p.c_program_id),
      '{}'
    )                                                                        AS c_aeon_required_instruments,
    -- Key for the AEON/multi-facility object: null unless the proposal is in the
    -- program, so the GraphQL object is null rather than an empty shell.
    CASE WHEN p.c_aeon_multi_facility                  THEN c_program_id END AS c_aeon_multi_facility_id,
    -- Key for the nullable explicit time request: null when no request was
    -- stated, so the GraphQL object is null rather than a zero TimeSpan.
    CASE WHEN p.c_time_request IS NOT NULL              THEN c_program_id END AS c_time_request_id,
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

-- v_program selects p.*, so it is recreated to pick up the ceiling.  Body copied
-- from V1305, plus the two summaries.
DROP VIEW v_program;

CREATE VIEW v_program AS
  select
    q.*,
    coalesce(q.c_explicit_status, q.c_default_status) as c_status
  from (
    select
      p.*,
      coalesce(rc.c_resource_count, 0) as c_resource_count,
      -- Summaries of what the program's observations ask for.  Present,
      -- non-calibration observations only; calibrations never ask for either.
      (select coalesce(max(o.c_too_activation), 'none'::e_too_activation)
         from t_observation o
        where o.c_program_id = p.c_program_id
          and o.c_existence = 'present'
          and o.c_calibration_role is null) as c_max_too_activation,
      (select coalesce(max(o.c_scheduling_mode), 'unconstrained'::e_scheduling_mode)
         from t_observation o
        where o.c_program_id = p.c_program_id
          and o.c_existence = 'present'
          and o.c_calibration_role is null) as c_max_scheduling_mode,
      (case
         when (now() at time zone 'UTC')::date between p.c_active_start and p.c_active_end then 'active'
         else 'inactive'
       end)::d_tag as c_default_status
    from t_program p
    left join t_program_resource_count rc on rc.c_program_id = p.c_program_id
  ) q;


-- Body copied from V1276, reading the ceiling from the program.  Lowering it
-- clears Ready from observations whose outstanding request it no longer covers,
-- and the withdrawal arm of too_trigger_track_ready() closes the request out.
-- Clearing it lifts the restriction, so withdraws nothing.
CREATE OR REPLACE FUNCTION too_trigger_ceiling_withdraw()
  RETURNS trigger AS $$
BEGIN
  UPDATE t_observation o
     SET c_workflow_user_state = NULL
   WHERE o.c_workflow_user_state = 'ready'
     AND EXISTS (
       SELECT 1
         FROM t_too_trigger t
        WHERE t.c_observation_id  = o.c_observation_id
          AND t.c_program_id      = NEW.c_program_id
          AND t.c_status          = 'requested'
          AND t.c_too_activation  > NEW.c_too_activation_ceiling
     );

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER too_trigger_ceiling_withdraw_trigger
  AFTER UPDATE OF c_too_activation_ceiling ON t_program
  FOR EACH ROW
  WHEN (NEW.c_too_activation_ceiling IS NOT NULL
        AND NEW.c_too_activation_ceiling IS DISTINCT FROM OLD.c_too_activation_ceiling)
  EXECUTE FUNCTION too_trigger_ceiling_withdraw();

-- The workflow is cached with the obscalc result, and an observation above a set
-- ceiling is Unapproved, so every observation in the program is recalculated
-- when the ceiling moves.
CREATE FUNCTION too_activation_ceiling_obscalc_invalidate()
  RETURNS trigger AS $$
BEGIN
  CALL invalidate_all_obscalc_for_program(NEW.c_program_id);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER too_activation_ceiling_invalidate_obscalc_trigger
  AFTER UPDATE OF c_too_activation_ceiling ON t_program
  FOR EACH ROW
  WHEN (NEW.c_too_activation_ceiling IS DISTINCT FROM OLD.c_too_activation_ceiling)
  EXECUTE FUNCTION too_activation_ceiling_obscalc_invalidate();

-------------------------------------------------------------------------------
-- Views over t_observation.
-------------------------------------------------------------------------------

-- Body copied from V1324, less c_is_splittable.
CREATE VIEW v_observation AS
  SELECT o.*,
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
  ) AS c_signal_to_noise_target_id,
  o.c_altair_mode AS c_configuration_altair_mode
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;

-- Body copied verbatim from V1320.
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
  o.c_scheduling_mode,
  o.c_blind_offset_target_id,
  b.c_sid_rv AS c_blind_rv,
  b.c_source_profile AS c_blind_source_profile,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile,
  COALESCE(t.c_is_signal_to_noise_target, false) AS c_is_signal_to_noise_target,
  o.c_altair_mode,
  o.c_altair_field_lens,
  o.c_altair_cass_rotator,
  o.c_altair_nd_filter
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

-------------------------------------------------------------------------------
-- Backfills.
-------------------------------------------------------------------------------

-- Which observations an existing request covers is computed in Scala (regions,
-- coordinates at the call's reference time), so it cannot be reproduced here.
-- The backfill is program-wide instead: each request gets the highest mode among
-- its program's observations.  The mode was never approved before, so this
-- grants each existing configuration what the program already uses anywhere; a
-- slight over-grant, confined to programs that predate this change, chosen so
-- that no observation loses an approval it had.
--
-- User triggers are disabled for the rewrite, as V1267 did: nothing a request
-- approves changes, so there is nothing to notify, no updated_at to bump, and no
-- obscalc result to invalidate.
ALTER TABLE t_configuration_request DISABLE TRIGGER USER;

UPDATE t_configuration_request r
   SET c_scheduling_mode = COALESCE(
         (SELECT MAX(o.c_scheduling_mode)
            FROM t_observation o
           WHERE o.c_program_id = r.c_program_id
             AND o.c_existence = 'present'
             AND o.c_calibration_role IS NULL),
         'unconstrained'::e_scheduling_mode
       );

ALTER TABLE t_configuration_request ENABLE TRIGGER USER;

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON TYPE e_scheduling_mode IS
  'What the Scheduler may do to an observation, as a ladder in which each value '
  'keeps every restriction below it and adds one.  Note that no_splitting may '
  'still be interrupted -- interrupting it abandons the visit rather than '
  'resuming it.';

COMMENT ON COLUMN t_observation.c_scheduling_mode IS
  'The observation''s scheduling mode.  Declared, and independent of the '
  'asterism and of the ToO activation but for the compatibility rule in '
  'too_activation_scheduling_mode_compatible.';

COMMENT ON COLUMN t_observation.c_too_activation IS
  'What this observation is permitted to disrupt.  Declared, not derived: an '
  'observation is a Target of Opportunity exactly when this is above ''none''.  '
  'May not exceed the program''s ceiling, if it has one.';

COMMENT ON COLUMN t_observation.c_has_too_target IS
  'Whether the asterism holds an undeleted opportunity target, maintained by '
  'too_target_track_asterism() and too_target_track_target().';

COMMENT ON COLUMN t_program.c_too_activation_ceiling IS
  'The most disruptive ToO activation the program''s observations may declare; '
  'null for no restriction.  Acceptance fills in a null with too_activation_max(); '
  'staff may set or clear it.';
