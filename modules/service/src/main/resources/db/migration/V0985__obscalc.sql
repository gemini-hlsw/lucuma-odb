-- Obscalc Service

-- The obscalc state typically moves from 'ready' to 'pending' when something is
-- edited, then when the entry is picked up by an obscalc worker the state is
-- moved to 'calculating'.  When the calculation is complete, the state moves
-- to 'ready' again if there were no additional edits to the observation.
-- Otherwise, back to 'pending' to be picked up by a worker.
--
-- If there are possibly recoverable errors (calling the ITC) when calculating,
-- the state moves from 'calculating' to 'retry' to be attempted later.  An edit
-- while in 'retry' transitions the state to 'pending' immediately.

-- Other errors, like missing targets or observing mode, result in errors that
-- are stored as the result of the calculation.

CREATE TYPE e_obscalc_state AS ENUM(
  'pending',
  'retry',
  'calculating',
  'ready'
);

-- Creates the t_obscalc table to hold results that require expensive
-- calculation and ITC results.  At the moment this is basically the existing
-- t_execution_digest with some extra state, but more information (like time
-- accounting) could be added here.
CREATE TABLE t_obscalc(
  -- Primary Key (+ fk to t_observation)
  c_program_id           d_program_id        NOT NULL,
  c_observation_id       d_observation_id    NOT NULL,

  FOREIGN KEY (c_program_id, c_observation_id)
    REFERENCES t_observation(c_program_id, c_observation_id)
    ON DELETE CASCADE,
  CONSTRAINT t_obscalc_pkey PRIMARY KEY (c_program_id, c_observation_id),

  c_obscalc_state        e_obscalc_state     NOT NULL DEFAULT 'pending',

  -- When the entry was last marked dirty.  This is compared to the value when
  -- the entry was picked up by a worker at the time the result is written. If
  -- unchanged, the state moves to 'ready'.  Otherwise, it moves back to 'pending'.
  c_last_invalidation    timestamp           NOT NULL DEFAULT NOW(),

  -- When the last result was written.
  c_last_update          timestamp           NOT NULL DEFAULT NOW(),

  -- If in 'retry' state, the retry will not be attempted before this time.
  c_retry_at             timestamp           NULL DEFAULT NULL,

  -- The number of attempts to perform the calculation that have failed since
  -- it was last 'pending'.
  c_failure_count        int4                NOT NULL DEFAULT 0 CHECK (c_failure_count >= 0),

  -- retry fields only apply to 'retry' and 'calculating'
  CONSTRAINT check_retry_fields_only_for_retry_state CHECK (
    (c_obscalc_state = 'retry' OR c_obscalc_state = 'calculating') OR
    (c_retry_at IS NULL AND c_failure_count = 0)
  ),

  -- when in a retry state, there must be a retry at time
  CONSTRAINT check_retry_at_defined_for_retry_state CHECK (
    (c_obscalc_state != 'retry') OR (c_retry_at IS NOT NULL)
  ),

  -- Error result, if any.
  c_odb_error            jsonb,

  -- Imaging ITC Result
  c_img_target_id        d_target_id         REFERENCES t_target(c_target_id) ON DELETE CASCADE,
  c_img_exposure_time    interval            CHECK (c_img_exposure_time >= interval '0 seconds'),
  c_img_exposure_count   int4                CHECK (c_img_exposure_count >= 0),
  c_img_wavelength       d_wavelength_pm     CHECK (c_img_wavelength >= 0),
  c_img_single_sn        numeric(10,3),      CHECK (c_img_single_sn >= 0),
  c_img_total_sn         numeric(10, 3)      CHECK (c_img_total_sn >= 0),

  -- Spectroscopy ITC Result
  c_spec_target_id        d_target_id        REFERENCES t_target(c_target_id) ON DELETE CASCADE,
  c_spec_exposure_time    interval           CHECK (c_spec_exposure_time >= interval '0 seconds'),
  c_spec_exposure_count   int4               CHECK (c_spec_exposure_count >= 0),
  c_spec_wavelength       d_wavelength_pm    CHECK (c_spec_wavelength >= 0),
  c_spec_single_sn        numeric(10,3)      CHECK (c_spec_single_sn >= 0),
  c_spec_total_sn         numeric(10, 3)     CHECK (c_spec_total_sn >= 0),

  -- Setup time
  c_full_setup_time      interval            CHECK (c_full_setup_time >= interval '0 seconds'),
  c_reacq_setup_time     interval            CHECK (c_reacq_setup_time >= interval '0 seconds'),

  -- Acquisition Digest
  c_acq_obs_class        e_obs_class,
  c_acq_non_charged_time interval            CHECK (c_acq_non_charged_time >= interval '0 seconds'),
  c_acq_program_time     interval            CHECK (c_acq_program_time >= interval '0 seconds'),
  c_acq_offsets          int8[][],
  c_acq_atom_count       int4                CHECK (c_acq_atom_count >= 0),
  c_acq_execution_state  e_execution_state,

  -- Science Digest
  c_sci_obs_class        e_obs_class,
  c_sci_non_charged_time interval            CHECK (c_sci_non_charged_time >= interval '0 seconds'),
  c_sci_program_time     interval            CHECK (c_sci_program_time >= interval '0 seconds'),
  c_sci_offsets          int8[][],
  c_sci_atom_count       int4                CHECK (c_sci_atom_count >= 0),
  c_sci_execution_state  e_execution_state
);

-- Invalidates the obscalc results for the given observation, moving the
-- calculation state to 'pending' so that it may be picked up.  If an
-- observation is invalidated while it is updating ('calculating') then it is not
-- reset to 'pending' because we don't want it to get picked up by a worker.  The
-- last invalidation time is updated though so that at the end of the
-- calculation we know whether the result is then 'ready' (c_last_invalidation
-- time unchanged) or else should go back to 'pending' (has newer invalidation).
CREATE PROCEDURE invalidate_obscalc(
  observation_id d_observation_id
) LANGUAGE plpgsql AS $$
DECLARE
  program_id    d_program_id;
  current_state e_obscalc_state;
BEGIN
  SELECT c_program_id INTO program_id FROM t_observation WHERE c_observation_id = observation_id;

  -- Exit early if the observation no longer exists, which happens when
  -- calibration observations are truly good and gone.
  IF NOT FOUND THEN
    RETURN;
  END IF;

  -- Try to insert. If this is the first time the observation has been modified
  -- then it will succeed but otherwise we'll do nothing.
  INSERT INTO t_obscalc (c_program_id, c_observation_id)
  VALUES (program_id, observation_id)
  ON CONFLICT ON CONSTRAINT t_obscalc_pkey DO NOTHING;

  -- Get the current state and lock the row.
  SELECT c_obscalc_state INTO current_state
  FROM t_obscalc
  WHERE c_observation_id = observation_id
  FOR UPDATE;

  -- Update calculation state to 'pending' if not currently executing.  Always
  -- set the invalidation timestamp.
  IF current_state = 'calculating' :: e_obscalc_state THEN
    UPDATE t_obscalc
    SET c_last_invalidation = now(),
        c_failure_count     = 0,
        c_retry_at          = NULL
    WHERE c_observation_id = observation_id;
  ELSE
    UPDATE t_obscalc
    SET c_last_invalidation = now(),
        c_failure_count     = 0,
        c_retry_at          = NULL,
        c_obscalc_state     = 'pending' :: e_obscalc_state
    WHERE c_observation_id = observation_id;
  END IF;
END;
$$;

-- When we transition from Pass or null to either Fail or Usable or vice versa,
-- invalidate the obscalc result.  A failing dataset means the corresponding
-- step has to be re-executed.
CREATE OR REPLACE FUNCTION dataset_qa_state_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF (OLD.c_qa_state IS DISTINCT FROM NEW.c_qa_state) AND (
    ((OLD.c_qa_state IS NULL OR OLD.c_qa_state = 'Pass') AND (NEW.c_qa_state IN ('Fail', 'Usable')))
    OR
    ((OLD.c_qa_state IN ('Fail', 'Usable')) AND (NEW.c_qa_state IS NULL OR NEW.c_qa_state = 'Pass'))
  ) THEN
    CALL invalidate_obscalc(NEW.c_observation_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER dataset_qa_state_invalidate_trigger
  AFTER UPDATE OF c_qa_state ON t_dataset
  FOR EACH ROW
  EXECUTE FUNCTION dataset_qa_state_invalidate();

-- When a step is marked complete invalidate the obscalc result.
CREATE OR REPLACE FUNCTION step_record_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  observation_id d_observation_id;
BEGIN
   SELECT c_observation_id INTO observation_id
   FROM t_atom_record
   WHERE c_atom_id = NEW.c_atom_id;

   CALL invalidate_obscalc(observation_id);

   RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- marked complete (or marked incomplete having previously been marked complete).
CREATE TRIGGER step_record_invalidate_trigger
  AFTER UPDATE ON t_step_record
  FOR EACH ROW
  WHEN (num_nulls(NEW.c_completed, OLD.c_completed) = 1)
    EXECUTE PROCEDURE step_record_invalidate();

-- When a table with a c_observation_id is updated, invalidate its calculation
CREATE OR REPLACE FUNCTION obsid_obscalc_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    CALL invalidate_obscalc(OLD.c_observation_id);
  ELSIF TG_OP IN ('INSERT', 'UPDATE') THEN
    IF TG_OP = 'INSERT' OR ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
      CALL invalidate_obscalc(NEW.c_observation_id);
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DO $$
DECLARE
  t text;
BEGIN
  FOREACH t IN ARRAY ARRAY[
    't_observation',
    't_asterism_target',
    't_gmos_north_long_slit',
    't_gmos_south_long_slit',
    't_flamingos_2_long_slit'
  ]
  LOOP
    EXECUTE format($f$
      CREATE TRIGGER %I_invalidate_trigger
      AFTER INSERT OR UPDATE OR DELETE ON %I
      FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate()
    $f$, substring(t from 3), t);
  END LOOP;
END;
$$;

-- When a target is updated, invalidate all the observations that use it
CREATE OR REPLACE FUNCTION target_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  target record;
  obs_id d_observation_id;
BEGIN
  target := COALESCE(NEW, OLD);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    FOR obs_id IN
      SELECT c_observation_id
        FROM t_asterism_target
       WHERE c_program_id = target.c_program_id
         AND c_target_id  = target.c_target_id
    LOOP
      CALL invalidate_obscalc(obs_id);
    END LOOP;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER target_invalidate_trigger
  AFTER UPDATE ON t_target
  FOR EACH ROW
  EXECUTE FUNCTION target_invalidate();

-- Notify when the obscalc state is updated.
CREATE OR REPLACE FUNCTION ch_obscalc_update()
  RETURNS trigger AS $$
DECLARE
  obscalc record;
BEGIN
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    obscalc := COALESCE(NEW, OLD);
    PERFORM pg_notify(
      'ch_obscalc_update',
      array_to_string(ARRAY[
        obscalc.c_observation_id,
        obscalc.c_program_id,
        COALESCE(OLD.c_obscalc_state::text, 'null'),
        COALESCE(NEW.c_obscalc_state::text, 'null'),
        TG_OP
      ], ',')
    );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_obscalc_update_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_obscalc
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_obscalc_update();

-- N.B. These shold be removed in a future migration.
-- DROP TRIGGER dataset_qa_state_update_trigger ON t_dataset;
-- DROP FUNCTION clear_execution_digest_on_qa_state_change;
-- DROP TRIGGER ch_observation_edit_execution_digest_trigger;
-- DROP TRIGGER delete_execution_digest_trigger;
-- DROP FUNCTION delete_execution_digest;

-- Initialize the t_obscalc table to set all existing observations to pending.
INSERT INTO t_obscalc (
  c_observation_id,
  c_program_id,
  c_obscalc_state,
  c_last_invalidation
)
SELECT
  o.c_observation_id,
  o.c_program_id,
  'pending'::e_obscalc_state,
  now()
FROM t_observation o
ON CONFLICT DO NOTHING;