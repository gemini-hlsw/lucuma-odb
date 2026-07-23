-- Root-cause fix for the deadlock class that produced V1212 and V1217.  Two
-- changes, in order of importance:
--
--   1. Stop taking a lock mode that fights the FK machinery -- "don't take a lock
--      that strong in the first place" instead of "acquire the strong lock first".
--   2. Move the mutex (and the c_step_execution_order counter) off t_observation
--      onto a dedicated per-observation row.  See 1b below.
--
-- WHY (1) WORKS
--
-- The execution-information triggers need mutual exclusion only among THEMSELVES:
-- one ongoing step per observation, and a serialized c_step_execution_order
-- increment.  They were written with FOR UPDATE, which is the strongest row lock
-- and -- critically -- the ONLY mode that conflicts with FOR KEY SHARE:
--
--   requested \ held | KEY SHARE | SHARE | NO KEY UPDATE | UPDATE
--   KEY SHARE        |    ok     |  ok   |      ok       |  CONFLICT
--   NO KEY UPDATE    |    ok     | CONF  |    CONFLICT   |  CONFLICT
--   UPDATE           | CONFLICT  | CONF  |    CONFLICT   |  CONFLICT
--
-- FOR KEY SHARE is exactly what PostgreSQL takes on t_observation to enforce the
-- foreign key of every child row inserted (t_execution_event, t_visit, t_atom,
-- t_dataset, t_sequence_materialization, ... -- 46 tables reference it).  So
-- FOR UPDATE was the one choice guaranteed to fight the FK machinery, on the most
-- referenced row in the schema.
--
-- FOR NO KEY UPDATE still conflicts with itself, so the triggers serialize
-- exactly as before -- the invariant is preserved.  But it does NOT conflict with
-- FOR KEY SHARE, so the upgrade KEY SHARE -> NO KEY UPDATE can never deadlock:
--
--   A: holds KEY SHARE(obs), wants NO KEY UPDATE -> B holds only KEY SHARE -> GRANTED
--   B: holds KEY SHARE(obs), wants NO KEY UPDATE -> conflicts with A's -> waits -> proceeds
--
-- No cycle, and no ordering discipline required.  This is why the approach scales:
-- V1217 must be repeated for every FK-bearing child table (and for every one added
-- in future), whereas weakening the mode fixes all of them at once.
--
-- SECONDARY BENEFIT
--
-- While a FOR UPDATE was held, EVERY insert of ANY child row for that observation
-- blocked -- a throughput cost nobody was measuring.  Under FOR NO KEY UPDATE
-- those inserts proceed freely; only the trigger bodies serialize.

-- 1. The BEFORE INSERT trigger from V1217 is no longer needed: there is no longer
--    an upgrade to lose a race on.  (Harmless if left in place, but it takes an
--    unnecessary exclusive lock on every event insert, which is the very
--    contention this migration removes.)
DROP TRIGGER IF EXISTS lock_observation_for_execution_event_trigger ON t_execution_event;
DROP FUNCTION IF EXISTS lock_observation_for_execution_event();

-- 1b. Move the mutex (and its counter) off t_observation entirely.
--
-- t_observation was only ever a convenient anchor: the invariant being protected
-- lives in t_step_execution, not in the observation row.  Two costs came with
-- that choice:
--
--   * FALSE SHARING.  NO KEY UPDATE conflicts with itself, and an ordinary
--     UPDATE of any non-key observation column takes exactly that mode.  So
--     editing an observation's title or mode params contended with step-event
--     processing, and vice versa, for no reason.
--
--   * WRITE CHURN ON THE HOTTEST ROW.  c_step_execution_order lived on
--     t_observation, so every new step UPDATEd the widest, most-read,
--     46-times-FK-referenced row in the schema just to bump a counter -- one
--     dead tuple per step, with the attendant bloat and vacuum pressure.
--
-- Both go away by giving each observation a dedicated row whose only job is to
-- carry the execution counter and serve as the execution mutex.  Nothing
-- FK-references this table, so its rows are never KEY SHARE'd by child inserts
-- and the mutex is a clean, uncontended acquisition rather than an upgrade.
--
-- The canonical lock order becomes:
--
--     t_observation_execution  ->  t_obscalc  ->  t_visit
--
-- and t_observation is no longer locked or written by event processing at all.
CREATE TABLE t_observation_execution (
  c_observation_id       d_observation_id PRIMARY KEY
                           REFERENCES t_observation (c_observation_id) ON DELETE CASCADE,
  c_step_execution_order integer NOT NULL DEFAULT 0
);

-- Carry every existing observation's counter forward.
INSERT INTO t_observation_execution (c_observation_id, c_step_execution_order)
SELECT c_observation_id, c_step_execution_order FROM t_observation;

-- The old column is now dead.  Drop it so any lingering reference fails loudly
-- rather than silently reading a stale zero.  Nothing FK-references or reads it
-- (no Scala reference; only v_observation surfaces it, implicitly via `SELECT
-- o.*`), and nothing is layered on v_observation -- so this is the same
-- DROP VIEW / recreate cycle every migration that touches the view already does.
-- Because the view is `SELECT o.*`, recreating it after the column is gone drops
-- the column from the view automatically.  View body copied verbatim from V1206.
DROP VIEW v_observation;

ALTER TABLE t_observation DROP COLUMN c_step_execution_order;

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
  -- Scalar subquery: at most one flagged target per observation is guaranteed
  -- by the partial unique index i_asterism_single_sn_target. No LIMIT here on
  -- purpose -- if that invariant were ever violated, PostgreSQL raises "more
  -- than one row returned by a subquery used as an expression", surfacing the
  -- bug rather than hiding it.
  (
    SELECT a.c_target_id
    FROM t_asterism_target a
    WHERE a.c_observation_id = o.c_observation_id
      AND a.c_is_signal_to_noise_target
  ) AS c_signal_to_noise_target_id
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;


-- Every observation gets its execution row at creation, so the mutex always has
-- something to lock.
CREATE FUNCTION create_observation_execution()
  RETURNS trigger AS $$
BEGIN
  INSERT INTO t_observation_execution (c_observation_id)
  VALUES (NEW.c_observation_id)
  ON CONFLICT DO NOTHING;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER create_observation_execution_trigger
  AFTER INSERT ON t_observation
  FOR EACH ROW EXECUTE FUNCTION create_observation_execution();

-- THE mutex.  Every writer of observation-scoped execution information takes
-- this, first, and holds it to commit.
--
-- FOR NO KEY UPDATE rather than FOR UPDATE: nothing FK-references this table
-- today so the two would behave identically, but the weaker mode is what is
-- actually needed (it conflicts with itself, which is the whole requirement) and
-- it stays correct if something ever does reference it.
--
-- The row is created with the observation, so the NOT FOUND branch should be
-- dead.  It exists because a missing row would otherwise mean "locked nothing"
-- -- silently no mutual exclusion, the worst possible failure for a mutex.  The
-- INSERT ... SELECT is deliberate: if the observation itself is gone (deleted
-- calibrations), it inserts nothing and the whole call is a harmless no-op,
-- matching invalidate_obscalc's behaviour rather than raising a FK violation.
CREATE FUNCTION lock_observation_execution(p_observation_id d_observation_id)
  RETURNS void AS $$
BEGIN
  PERFORM 1 FROM t_observation_execution
   WHERE c_observation_id = p_observation_id
     FOR NO KEY UPDATE;

  IF NOT FOUND THEN
    INSERT INTO t_observation_execution (c_observation_id)
    SELECT c_observation_id FROM t_observation WHERE c_observation_id = p_observation_id
    ON CONFLICT DO NOTHING;

    PERFORM 1 FROM t_observation_execution
     WHERE c_observation_id = p_observation_id
       FOR NO KEY UPDATE;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- 2. Step / dataset events.  Body identical to V1100 except for how the mutex is
--    taken and where the execution-order counter lives.
CREATE OR REPLACE FUNCTION update_execution_information_for_step_event()
  RETURNS TRIGGER AS $$
DECLARE
  sequence_type    e_sequence_type;
  step_stage_state d_tag;
  new_order        integer;
BEGIN

  -- What is the sequence type we're working with?
  SELECT c_sequence_type INTO STRICT sequence_type
  FROM t_atom
  WHERE c_atom_id = NEW.c_atom_id;

  -- What is the execution state according to the step stage? (For step events,
  -- otherwise this is NULL.)
  SELECT s.c_execution_state
  INTO step_stage_state
  FROM t_step_stage_execution_state s
  WHERE s.c_step_stage = NEW.c_step_stage;

  -- Serialize with the other execution-information writers.  Note this locks
  -- t_observation_execution, NOT t_observation: the INSERT INTO t_execution_event
  -- that fired this trigger already holds FOR KEY SHARE on the observation row,
  -- and taking a conflicting mode on it is the deadlock this migration removes.
  PERFORM lock_observation_execution(NEW.c_observation_id);

  -- Ensure only one ongoing step in this observation.
  UPDATE t_step_execution se
     SET c_execution_state = 'abandoned'
   WHERE c_observation_id = NEW.c_observation_id
     AND c_step_id       <> NEW.c_step_id
     AND c_execution_state = 'ongoing';

  IF NOT EXISTS (
    SELECT 1 FROM t_step_execution WHERE c_step_id = NEW.c_step_id
  ) THEN

    UPDATE t_observation_execution
       SET c_step_execution_order = c_step_execution_order + 1
     WHERE c_observation_id = NEW.c_observation_id
    RETURNING c_step_execution_order INTO new_order;

    INSERT INTO t_step_execution (
      c_step_id,
      c_observation_id,
      c_sequence_type,
      c_visit_id,
      c_execution_state,
      c_execution_order,
      c_first_event_time,
      c_last_event_time
    )
    VALUES (
      NEW.c_step_id,
      NEW.c_observation_id,
      sequence_type,
      NEW.c_visit_id,
      COALESCE(step_stage_state, 'ongoing'), -- born ongoing
      new_order,
      NEW.c_received,
      NEW.c_received
    );

  ELSE

    UPDATE t_step_execution e
    SET
      c_visit_id         = NEW.c_visit_id, -- we include the visit in order to fail (in validate_step_execution_update()) if it has changed
      c_first_event_time = least(e.c_first_event_time,   NEW.c_received),
      c_last_event_time  = greatest(e.c_last_event_time, NEW.c_received),
      c_execution_state  =
        CASE
          -- If cur state (cs) in a terminal execution state, we stay there.
          WHEN cs.c_terminal THEN e.c_execution_state

          -- If new state (ns) is a terminal state, go ahead
          WHEN ns.c_terminal THEN step_stage_state

          -- Otherwise we're executing
          ELSE 'ongoing'
        END
    FROM t_step_execution_state cs
    LEFT JOIN t_step_execution_state ns ON ns.c_tag = step_stage_state
    WHERE e.c_step_id = NEW.c_step_id
      AND cs.c_tag    = e.c_execution_state;

  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- 3. Sequence (abort) events.
CREATE OR REPLACE FUNCTION update_execution_information_for_sequence_event()
  RETURNS TRIGGER AS $$
BEGIN

  IF NEW.c_sequence_command = 'abort' THEN

    PERFORM lock_observation_execution(NEW.c_observation_id);

    UPDATE t_step_execution se
    SET c_execution_state = 'abandoned'
    WHERE se.c_observation_id = NEW.c_observation_id
      AND se.c_execution_state = 'ongoing';

  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- 4. Visit recording.
CREATE OR REPLACE FUNCTION update_execution_information_for_visit()
  RETURNS TRIGGER AS $$
BEGIN

  PERFORM lock_observation_execution(NEW.c_observation_id);

  UPDATE t_step_execution se
  SET c_execution_state = 'abandoned'
  WHERE se.c_execution_state = 'ongoing'
    AND se.c_visit_id <> NEW.c_visit_id;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- 5. Time-accounting invalidation (V1212).  The lock here exists purely to pin the
--    canonical order t_observation_execution -> t_obscalc -> t_visit.  It must be
--    the SAME mutex the other writers take, or the ordering guarantee is empty.
CREATE OR REPLACE PROCEDURE invalidate_visit_time_accounting(
  visit_id       d_visit_id,
  observation_id d_observation_id
) LANGUAGE plpgsql AS $$
BEGIN
  PERFORM lock_observation_execution(observation_id);
  CALL invalidate_obscalc(observation_id);
  UPDATE t_visit SET c_ta_invalidation = now() WHERE c_visit_id = visit_id;
END;
$$;

-- 6. Sequence regeneration.
--
--    The argument that NO KEY UPDATE suffices: every path that could interfere
--    takes the same t_observation_execution mutex, so they still mutually
--    exclude.  A competing sequence regeneration calls this
--    procedure itself; a competing execution event goes through
--    update_execution_information_for_step_event (which creates the
--    t_step_execution rows this procedure's NOT EXISTS checks read).  What is
--    given up is blocking inserts into child tables that take no explicit lock at
--    all (t_dataset, t_atom_digest, ...), none of which this procedure reads.
CREATE OR REPLACE PROCEDURE abandon_ongoing_and_delete_unexecuted_steps(
  p_observation_id d_observation_id,
  p_sequence_type  e_sequence_type
) AS $$
BEGIN

  PERFORM lock_observation_execution(p_observation_id);

  -- Abandon ongoing steps.
  UPDATE t_step_execution se
     SET c_execution_state = 'abandoned'
   WHERE se.c_execution_state = 'ongoing'
     AND se.c_observation_id = p_observation_id
     AND se.c_sequence_type  = p_sequence_type;

  -- Delete steps that have no execution information (i.e., unexecuted steps)
  DELETE FROM t_step s
  USING t_atom a
  WHERE s.c_atom_id = a.c_atom_id
    AND a.c_observation_id = p_observation_id
    AND a.c_sequence_type  = p_sequence_type
    AND NOT EXISTS (
      SELECT 1 FROM t_step_execution se WHERE se.c_step_id = s.c_step_id
    );

  -- Delete atoms with no (executed) steps -- all unexecuted steps have been deleted
  -- so only executed steps remain.
  DELETE FROM t_atom a
  WHERE a.c_observation_id = p_observation_id
    AND a.c_sequence_type  = p_sequence_type
    AND NOT EXISTS (
      SELECT 1
        FROM t_step s
        JOIN t_step_execution se USING (c_step_id)
       WHERE s.c_atom_id = a.c_atom_id
    );

END;
$$ LANGUAGE plpgsql;
