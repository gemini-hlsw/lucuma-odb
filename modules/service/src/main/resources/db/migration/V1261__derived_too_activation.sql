-- Makes the observation's ToO activation derived rather than declared.
--
-- Until now a PI set c_too_activation directly, independently of the asterism,
-- so an observation could claim to be a rapid Target of Opportunity while
-- pointing at an ordinary sidereal target -- and, worse, a genuine ToO stopped
-- looking like one the moment its target was resolved.
--
-- Now an observation is a ToO exactly when its asterism holds an opportunity
-- target, and how disruptive it may be follows from its scheduling mode:
--
--   opportunity target?  scheduling mode                  activation
--   no                   any                              none
--   yes                  unconstrained | no_splitting     standard
--   yes                  uninterruptible                  rapid
--   yes                  interrupting                     interrupting
--
-- 'interrupting' additionally *requires* an opportunity target; an observation
-- carrying that mode without one is invalid rather than meaningful, and the
-- workflow rejects it.  It derives 'none' here, which is the safe direction: it
-- cannot reach a ready state, so it never executes and never interrupts
-- anything, and it must not raise the ceiling its program derives.
--
-- WHY A GENERATED COLUMN
--
-- c_too_activation becomes GENERATED ALWAYS ... STORED over two plain columns of
-- the same row, so it cannot drift from its inputs and any leftover write is a
-- hard error rather than a silent winner.  That costs a drop-and-re-add (a
-- column cannot be altered into a generated one), which is why v_observation and
-- the trigger come down and go back up below.
--
-- The asterism half is denormalized into c_has_too_target rather than joined on
-- demand.  too_trigger_track_ready has to stay a single-table row trigger, and
-- too_activation_ceiling_default has to stay a plain aggregate over
-- t_observation -- ObservationWorkflowService joins v_proposal for the effective
-- ceiling on every workflow calculation and must not grow an asterism join.

-------------------------------------------------------------------------------
-- Inputs to the derivation.
-------------------------------------------------------------------------------

-- Adding a column with a constant default is metadata-only (no table rewrite, no
-- triggers fired), so this can lead.  It is backfilled at the very end.
ALTER TABLE t_observation
  ADD COLUMN c_has_too_target boolean NOT NULL DEFAULT false;

-- Does this observation's asterism hold an opportunity target?  Deleted targets
-- do not count: v_generator_params and the rest of the pipeline already ignore
-- them, so an observation whose only ToO target was deleted is not a ToO.
CREATE FUNCTION observation_has_too_target(
  oid d_observation_id
) RETURNS boolean AS $$
  SELECT EXISTS (
    SELECT 1
      FROM t_asterism_target a
      JOIN t_target t ON t.c_target_id = a.c_target_id
     WHERE a.c_observation_id = oid
       AND t.c_existence = 'present'
       AND t.c_type = 'opportunity'::e_target_type
  );
$$ LANGUAGE sql STABLE;

-- The derivation itself.  IMMUTABLE because a generated column requires it, and
-- honestly so: it reads nothing but its arguments.
CREATE FUNCTION too_activation(
  has_too_target boolean,
  mode           e_scheduling_mode
) RETURNS e_too_activation AS $$
  SELECT CASE
    WHEN NOT has_too_target                          THEN 'none'::e_too_activation
    WHEN mode = 'interrupting'::e_scheduling_mode    THEN 'interrupting'::e_too_activation
    WHEN mode = 'uninterruptible'::e_scheduling_mode THEN 'rapid'::e_too_activation
    ELSE                                                  'standard'::e_too_activation
  END;
$$ LANGUAGE sql IMMUTABLE;

-------------------------------------------------------------------------------
-- Re-make c_too_activation as a generated column.
-------------------------------------------------------------------------------

-- The trigger names c_too_activation in its UPDATE OF list, and v_observation is
-- `SELECT o.*` so it depends on the column by name.  Both are recreated below.
DROP TRIGGER too_trigger_track_ready_trigger ON t_observation;
DROP VIEW v_observation;

ALTER TABLE t_observation DROP COLUMN c_too_activation;

ALTER TABLE t_observation
  ADD COLUMN c_too_activation e_too_activation NOT NULL
    GENERATED ALWAYS AS (too_activation(c_has_too_target, c_scheduling_mode)) STORED;

-- Body copied verbatim from V1260; the column list moves c_too_activation to the
-- end, which is invisible to Grackle and to Skunk's explicit column lists.
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

-- As V1246, except for the UPDATE OF list.  A generated column can never appear
-- in an UPDATE's SET clause, and UPDATE OF matches the statement's target
-- columns rather than what actually changed -- so `UPDATE OF c_too_activation`
-- would silently never fire again.  Watch the two inputs instead.
CREATE TRIGGER too_trigger_track_ready_trigger
  AFTER INSERT OR UPDATE OF c_workflow_user_state, c_scheduling_mode, c_has_too_target ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION too_trigger_track_ready();

-------------------------------------------------------------------------------
-- Keeping c_has_too_target in step with the asterism.
-------------------------------------------------------------------------------

-- Recomputes the flag for one observation.  The IS DISTINCT FROM guard keeps a
-- no-op edit from firing too_trigger_track_ready, which would otherwise see an
-- UPDATE OF c_has_too_target on every asterism touch.
CREATE FUNCTION refresh_has_too_target(oid d_observation_id) RETURNS void AS $$
  UPDATE t_observation o
     SET c_has_too_target = observation_has_too_target(oid)
   WHERE o.c_observation_id = oid
     AND o.c_has_too_target IS DISTINCT FROM observation_has_too_target(oid);
$$ LANGUAGE sql;

CREATE FUNCTION too_target_track_asterism() RETURNS trigger AS $$
BEGIN
  PERFORM refresh_has_too_target(COALESCE(NEW.c_observation_id, OLD.c_observation_id));
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER too_target_track_asterism_trigger
  AFTER INSERT OR DELETE OR UPDATE OF c_target_id ON t_asterism_target
  FOR EACH ROW
  EXECUTE FUNCTION too_target_track_asterism();

-- A target can become (or stop being) an opportunity target in place, and it can
-- be soft-deleted, so the observations holding it have to be revisited.
CREATE FUNCTION too_target_track_target() RETURNS trigger AS $$
DECLARE
  oid d_observation_id;
BEGIN
  FOR oid IN
    SELECT a.c_observation_id
      FROM t_asterism_target a
     WHERE a.c_target_id = COALESCE(NEW.c_target_id, OLD.c_target_id)
  LOOP
    PERFORM refresh_has_too_target(oid);
  END LOOP;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER too_target_track_target_trigger
  AFTER DELETE OR UPDATE OF c_type, c_existence ON t_target
  FOR EACH ROW
  EXECUTE FUNCTION too_target_track_target();

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON COLUMN t_observation.c_has_too_target IS
  'Whether the asterism holds an undeleted opportunity target, maintained by '
  'too_target_track_asterism() and too_target_track_target().  Denormalized so '
  'that the ToO activation and the trigger stay single-table.';

COMMENT ON COLUMN t_observation.c_too_activation IS
  'Derived: what this observation is permitted to disrupt, from whether it holds '
  'an opportunity target and its scheduling mode.  Not settable.';

-------------------------------------------------------------------------------
-- Backfill, last.
-------------------------------------------------------------------------------

-- Every row currently reads false, so the re-added generated column computed
-- 'none' for all of them.  This UPDATE queues deferred trigger events, so no
-- ALTER may follow it -- and it deliberately fires too_trigger_track_ready, so
-- an observation that is Ready with a genuine ToO target gets its trigger row.
UPDATE t_observation o
   SET c_has_too_target = true
 WHERE observation_has_too_target(o.c_observation_id);

-- The converse is not covered by that UPDATE: an observation that *declared* a
-- ToO activation with an ordinary asterism was previously triggerable and may
-- hold a live request, but derives 'none' now and is no longer a ToO.  Nothing
-- updates its row, so withdraw those explicitly rather than leaving a trigger
-- that no observation backs.
UPDATE t_too_trigger t
   SET c_status = 'withdrawn'
  FROM t_observation o
 WHERE o.c_observation_id = t.c_observation_id
   AND t.c_status = 'requested'
   AND o.c_too_activation = 'none'::e_too_activation;
