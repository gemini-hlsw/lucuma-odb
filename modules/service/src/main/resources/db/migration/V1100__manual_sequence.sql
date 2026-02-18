-- Create an atom table.  This will store both executed and future atoms, but
-- only when the first visit for an observation is created.  Before then we
-- continue generating the sequence on the fly and nothing is written to the
-- database.
CREATE TABLE t_atom (
  c_atom_id          d_atom_id        PRIMARY KEY,
  c_instrument       d_tag            NOT NULL,
  c_atom_index       integer          NOT NULL CHECK (c_atom_index > 0),

  UNIQUE (c_atom_id, c_instrument),

  c_observation_id   d_observation_id NOT NULL,
  c_sequence_type    e_sequence_type  NOT NULL,

  UNIQUE (c_observation_id, c_sequence_type, c_atom_index),

  c_visit_id         d_visit_id       REFERENCES t_visit(c_visit_id) ON DELETE CASCADE,

  c_description      text,

  c_first_event_time timestamp without time zone,
  c_last_event_time  timestamp without time zone

);

-- Copy the old t_atom_record data over to t_atom.
INSERT INTO t_atom (
  c_atom_id,
  c_instrument,
  c_atom_index,
  c_observation_id,
  c_sequence_type,
  c_visit_id,
  c_first_event_time,
  c_last_event_time
)
SELECT
  r2.c_atom_id,
  r2.c_instrument,
  r2.c_atom_index,
  r2.c_observation_id,
  r2.c_sequence_type,
  r2.c_visit_id,
  MIN(e.c_received) AS c_first_event_time,
  MAX(e.c_received) AS c_last_event_time
FROM (
  SELECT
    r.c_atom_id,
    r.c_instrument,
    r.c_observation_id,
    ROW_NUMBER() OVER (
      PARTITION BY r.c_observation_id
      ORDER BY r.c_created, r.c_atom_id
    ) AS c_atom_index,
    r.c_sequence_type,
    r.c_visit_id
  FROM t_atom_record r
) r2 -- for atom index calculation
LEFT JOIN t_execution_event e ON e.c_atom_id = r2.c_atom_id
GROUP BY -- for MIN/MAX aggregation
  r2.c_atom_id,
  r2.c_instrument,
  r2.c_atom_index,
  r2.c_observation_id,
  r2.c_sequence_type,
  r2.c_visit_id;

ALTER TABLE ONLY t_atom
  ADD CONSTRAINT t_atom_c_observation_id_c_instrument_fkey FOREIGN KEY (c_observation_id, c_instrument) REFERENCES t_observation(c_observation_id, c_instrument) ON DELETE CASCADE;

-- The visit id is assigned in t_atom when events arrive from Observe.  We check
-- here to make sure the visit is compatible (same observation) and that the
-- visit isn't being changed once initially set.
CREATE OR REPLACE FUNCTION validate_atom_visit()
RETURNS TRIGGER AS $$
DECLARE
  observation_id d_observation_id;
BEGIN
  -- Make sure the visit applies to the same observation as the atom.
  IF NEW.c_visit_id IS NOT NULL THEN
    SELECT v.c_observation_id INTO observation_id FROM t_visit v WHERE v.c_visit_id = NEW.c_visit_id;
    IF NEW.c_observation_id IS DISTINCT FROM observation_id THEN
      RAISE EXCEPTION 'atom % and visit % cannot have distinct observation ids (% and % respectively)', NEW.c_atom_id, NEW.c_visit_id, NEW.c_observation_id, observation_id
        USING ERRCODE = 'check_violation';
    END IF;
  END IF;

  -- Don't allow the visit to change.
  IF OLD.c_visit_id IS NOT NULL AND NEW.c_visit_id IS DISTINCT FROM OLD.c_visit_id THEN
    RAISE EXCEPTION 'c_visit_id is write-once: cannot change from % to %', OLD.c_visit_id, NEW.c_visit_id
    USING ERRCODE = 'check_violation';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER validate_atom_visit_trigger
BEFORE UPDATE OF c_visit_id ON t_atom
FOR EACH ROW
EXECUTE FUNCTION validate_atom_visit();


-- A trigger to keep the first/last event data accurate.
CREATE OR REPLACE FUNCTION update_atom_event_times()
  RETURNS TRIGGER AS $$
BEGIN

  UPDATE t_atom
     SET c_first_event_time = least(coalesce(c_first_event_time, NEW.c_received), NEW.c_received),
         c_last_event_time  = greatest(coalesce(c_last_event_time, NEW.c_received), NEW.c_received)
   WHERE c_atom_id = NEW.c_atom_id;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_atom_event_times_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW
  WHEN (NEW.c_atom_id IS NOT NULL)
    EXECUTE PROCEDURE update_atom_event_times();


-- Add a breakpoint enum.  We can now store breakpoints.
CREATE TYPE e_breakpoint AS enum(
  'enabled',
  'disabled'
);

CREATE TABLE t_step (
  c_step_id          d_step_id     PRIMARY KEY,
  c_atom_id          d_atom_id     NOT NULL REFERENCES t_atom(c_atom_id) DEFERRABLE INITIALLY DEFERRED,
  c_instrument       d_tag         NOT NULL,
  c_step_type        e_step_type   NOT NULL,
  c_step_index       integer       NOT NULL CHECK (c_step_index > 0),

  UNIQUE (c_step_id, c_instrument),
  UNIQUE (c_step_id, c_step_type),
  UNIQUE (c_atom_id, c_step_index),

  c_observe_class    e_obs_class   NOT NULL,
  c_time_estimate    interval      NOT NULL DEFAULT '00:00:00'::interval,
  c_execution_state  d_tag         NOT NULL REFERENCES t_step_execution_state(c_tag) DEFAULT ('not_started'::character varying)::d_tag,
  c_offset_p         d_angle_µas   NOT NULL DEFAULT 0,
  c_offset_q         d_angle_µas   NOT NULL DEFAULT 0,
  c_guide_state      e_guide_state NOT NULL DEFAULT 'enabled'::e_guide_state,

  c_breakpoint       e_breakpoint  NOT NULL DEFAULT 'disabled'::e_breakpoint,

  c_first_event_time timestamp without time zone,
  c_last_event_time  timestamp without time zone
);

-- Copy the old t_step_record data over to t_step.
INSERT INTO t_step (
  c_step_id,
  c_atom_id,
  c_instrument,
  c_step_type,
  c_step_index,
  c_observe_class,
  c_time_estimate,
  c_execution_state,
  c_offset_p,
  c_offset_q,
  c_guide_state,
  c_first_event_time,
  c_last_event_time
)
SELECT
  r.c_step_id,
  r.c_atom_id,
  r.c_instrument,
  r.c_step_type,
  r.c_step_index,
  r.c_observe_class,
  r.c_time_estimate,
  r.c_execution_state,
  r.c_offset_p,
  r.c_offset_q,
  r.c_guide_state,
  r.c_first_event_time,
  r.c_last_event_time
FROM t_step_record r;

ALTER TABLE ONLY t_step
  ADD CONSTRAINT t_atom_c_atom_id_c_instrument_fkey FOREIGN KEY (c_atom_id, c_instrument) REFERENCES t_atom(c_atom_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

-- Create a view on the atom table.  This will correspond to the old atom record
-- table in that it only contains executed atoms.
CREATE VIEW v_atom_record AS
SELECT
  a.*,
  COALESCE(s.c_execution_state, 'completed')::d_tag AS c_execution_state
FROM t_atom a
LEFT JOIN (
  SELECT
    c_atom_id,
    CASE
      WHEN bool_and(c_execution_state IN ('completed', 'abandoned')) THEN 'completed'
      WHEN bool_and(c_execution_state = 'not_started')               THEN 'not_started'
      ELSE 'ongoing'
    END::d_tag AS c_execution_state
  FROM t_step
  GROUP BY c_atom_id
) s ON s.c_atom_id = a.c_atom_id
WHERE a.c_visit_id         IS NOT NULL
  AND a.c_first_event_time IS NOT NULL
  AND a.c_last_event_time  IS NOT NULL;

-- Update the step record view
DROP VIEW v_step_record;

-- Create a view on the step table.  This will correspond to the old step record
-- table in that it only contains executed atoms.
CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_atom_id,
  a.c_visit_id,
  s.c_step_index,
  s.c_instrument,
  s.c_step_type,
  s.c_observe_class,
  s.c_execution_state,
  s.c_time_estimate,
  s.c_breakpoint,
  s.c_first_event_time,
  s.c_last_event_time,
  g.c_gcal_continuum,
  g.c_gcal_ar_arc,
  g.c_gcal_cuar_arc,
  g.c_gcal_thar_arc,
  g.c_gcal_xe_arc,
  g.c_gcal_filter,
  g.c_gcal_diffuser,
  g.c_gcal_shutter,
  m.c_smart_gcal_type,
  s.c_offset_p,
  s.c_offset_q,
  s.c_guide_state,
  (SELECT MAX(c_qa_state) FROM t_dataset d WHERE d.c_step_id = s.c_step_id) AS c_qa_state
FROM
  t_step s
LEFT JOIN t_step_config_gcal g
  ON g.c_step_id = s.c_step_id
LEFT JOIN t_step_config_smart_gcal m
  ON m.c_step_id = s.c_step_id
INNER JOIN t_atom a
  ON a.c_atom_id = s.c_atom_id
WHERE a.c_visit_id         IS NOT NULL
  AND s.c_first_event_time IS NOT NULL
  AND s.c_last_event_time  IS NOT NULL
  AND s.c_execution_state != 'not_started'
ORDER BY
  s.c_step_id;


-- A trigger to keep the first/last event data accurate.
DROP TRIGGER update_step_record_event_times_trigger ON t_execution_event;
DROP FUNCTION update_step_record_event_times;

CREATE OR REPLACE FUNCTION update_step_event_times()
  RETURNS TRIGGER AS $$
BEGIN

  UPDATE t_step
     SET c_first_event_time = least(coalesce(c_first_event_time, NEW.c_received), NEW.c_received),
         c_last_event_time  = greatest(coalesce(c_last_event_time, NEW.c_received), NEW.c_received)
   WHERE c_step_id = NEW.c_step_id;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_step_event_times_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW
  WHEN (NEW.c_step_id IS NOT NULL)
    EXECUTE PROCEDURE update_step_event_times();

-- Sequence serialization coordination
CREATE TABLE t_sequence_materialization (
  c_observation_id d_observation_id PRIMARY KEY,
  c_created        timestamp        NOT NULL,
  c_updated        timestamp        NOT NULL
);

-- We'll say that every observation with an atom has been materialized.
INSERT INTO t_sequence_materialization (
  c_observation_id,
  c_created,
  c_updated
)
SELECT
  a.c_observation_id,
  MIN(a.c_first_event_time) AS c_created,
  MIN(a.c_first_event_time) AS c_updated
FROM t_atom a
GROUP BY a.c_observation_id;

-- When a new step is completed (or uncompleted if that ever happens), we need
-- to poke obscalc.
DROP TRIGGER  step_record_invalidate_trigger ON t_step_record;
DROP FUNCTION step_record_invalidate;

CREATE OR REPLACE FUNCTION step_obscalc_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  observation_id d_observation_id;
BEGIN
   SELECT c_observation_id INTO observation_id
   FROM t_atom
   WHERE c_atom_id = NEW.c_atom_id;

   CALL invalidate_obscalc(observation_id);

   RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER step_obscalc_invalidate_trigger
AFTER UPDATE ON t_step FOR EACH ROW WHEN (
  OLD.c_execution_state IS DISTINCT FROM NEW.c_execution_state AND (
    OLD.c_execution_state = 'completed' OR
    NEW.c_execution_state = 'completed'
  )
) EXECUTE FUNCTION step_obscalc_invalidate();

-- Deletes the observation's execution digest when one of its steps is marked
-- complete.
DROP TRIGGER delete_execution_digest_trigger ON t_step_record;
DROP FUNCTION delete_execution_digest;

CREATE OR REPLACE FUNCTION delete_execution_digest()
  RETURNS TRIGGER AS $$
BEGIN
   DELETE FROM t_execution_digest
     WHERE c_observation_id IN (
       SELECT DISTINCT o.c_observation_id
       FROM t_observation o
       JOIN t_atom a ON a.c_observation_id = o.c_observation_id
       WHERE a.c_atom_id = OLD.c_atom_id
     );
   RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Triggers the deletion of an observation's execution digest when a step is
-- marked complete (or marked incomplete having previously been marked complete).
CREATE TRIGGER delete_execution_digest_trigger
AFTER UPDATE ON t_step FOR EACH ROW
WHEN (
  OLD.c_execution_state IS DISTINCT FROM NEW.c_execution_state AND (
    OLD.c_execution_state = 'completed' OR
    NEW.c_execution_state = 'completed'
  )
) EXECUTE PROCEDURE delete_execution_digest();

-- Transfer the FK constraints.
ALTER TABLE t_dataset
  DROP CONSTRAINT t_dataset_c_step_id_fkey;

ALTER TABLE t_dataset
  ADD CONSTRAINT t_dataset_c_step_id_fkey FOREIGN KEY (c_step_id) REFERENCES t_step(c_step_id);

ALTER TABLE t_execution_event
  DROP CONSTRAINT t_execution_event_c_atom_id_fkey,
  DROP CONSTRAINT t_execution_event_c_step_id_fkey;

ALTER TABLE t_execution_event
  ADD CONSTRAINT t_execution_event_c_atom_id_fkey FOREIGN KEY (c_atom_id) REFERENCES t_atom(c_atom_id),
  ADD CONSTRAINT t_execution_event_c_step_id_fkey FOREIGN KEY (c_step_id) REFERENCES t_step(c_step_id);

ALTER TABLE t_step_config_gcal
  DROP CONSTRAINT t_step_config_gcal_c_step_id_c_step_type_fkey;

ALTER TABLE t_step_config_gcal
  ADD CONSTRAINT t_step_config_gcal_c_step_id_c_step_type_fkey FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step(c_step_id, c_step_type) DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_step_config_smart_gcal
  DROP CONSTRAINT t_step_config_smart_gcal_c_step_id_c_step_type_fkey;

ALTER TABLE t_step_config_smart_gcal
  ADD CONSTRAINT t_step_config_smart_gcal_c_step_id_c_step_type_fkey FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step(c_step_id, c_step_type) DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_flamingos_2_dynamic
  DROP CONSTRAINT t_flamingos_2_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_flamingos_2_dynamic
  ADD CONSTRAINT t_flamingos_2_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument) DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_gmos_north_dynamic
  DROP CONSTRAINT t_gmos_north_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_gmos_north_dynamic
  ADD CONSTRAINT t_gmos_north_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument) DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_gmos_south_dynamic
  DROP CONSTRAINT t_gmos_south_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_gmos_south_dynamic
  ADD CONSTRAINT t_gmos_south_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument) DEFERRABLE INITIALLY DEFERRED;

-- Rework the dataset visit, obs id, index assignment.  Before we knew that the
-- visit existed because datasets referred to steps that referred to atoms that
-- referred to visits and each layer had to exist and be assigned.  Now atoms
-- and steps are written before the visit is applied so the visit could in
-- theory be null at the time a new dataset is added.  We'll reject that.

DROP TRIGGER set_datatset_observation_trigger ON t_dataset;
DROP FUNCTION set_dataset_observation;

DROP TRIGGER set_initial_observation_reference_in_dataset_trigger ON t_dataset;
DROP FUNCTION set_initial_observation_reference_in_dataset;

DROP TRIGGER set_datatset_visit_trigger ON t_dataset;
DROP FUNCTION set_dataset_visit;

DROP TRIGGER check_dataset_visit_immutable_trigger ON t_dataset;
DROP FUNCTION check_dataset_visit_immutable;

CREATE FUNCTION initialize_dataset_before_insert()
RETURNS TRIGGER AS $$
DECLARE
  visit_id       d_visit_id;
  observation_id d_observation_id;
  step_index     int4;
BEGIN

  SELECT
    a.c_visit_id,
    a.c_observation_id,
    s.c_step_index
  INTO
    visit_id,
    observation_id,
    step_index
  FROM t_step s
  JOIN t_atom a ON a.c_atom_id = s.c_atom_id
  WHERE s.c_step_id = NEW.c_step_id;

  IF visit_id IS NULL THEN
    RAISE EXCEPTION
       USING
         ERRCODE = 'check_violation',
         MESSAGE = format(
           'Cannot insert dataset for step %s: atom has no visit assigned',
           NEW.c_step_id
         );
  END IF;

  NEW.c_visit_id       := visit_id;
  NEW.c_observation_id := observation_id;
  NEW.c_step_index     := step_index;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER initialize_dataset_before_insert_trigger
BEFORE INSERT ON t_dataset
FOR EACH ROW
EXECUTE FUNCTION initialize_dataset_before_insert();

-- Add execution state to the generator params
DROP VIEW v_generator_params;
CREATE OR REPLACE VIEW v_generator_params AS
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
  o.c_declared_complete,
  CASE
    WHEN o.c_declared_complete
      THEN 'completed'::e_execution_state

    WHEN NOT EXISTS (
      SELECT 1 FROM t_execution_event v WHERE v.c_observation_id = o.c_observation_id
    ) THEN 'not_started'::e_execution_state

    WHEN EXISTS (
      SELECT 1 FROM v_atom_record a
        WHERE a.c_observation_id = o.c_observation_id
          AND a.c_execution_state IN ('not_started', 'ongoing')
    ) THEN 'ongoing'::e_execution_state

    ELSE 'completed'::e_execution_state
  END AS c_execution_state,
  o.c_acq_reset_time,
  o.c_blind_offset_target_id,
  b.c_sid_rv AS c_blind_rv,
  b.c_source_profile AS c_blind_source_profile,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile
FROM
  t_observation o
LEFT JOIN t_target b ON c_target_id = o.c_blind_offset_target_id
LEFT JOIN LATERAL (
  SELECT t.c_target_id,
         t.c_sid_rv,
         t.c_source_profile
    FROM t_asterism_target a
    INNER JOIN t_target t
      ON a.c_target_id = t.c_target_id
     AND t.c_existence = 'present'
   WHERE a.c_observation_id = o.c_observation_id
) t ON TRUE
LEFT JOIN t_exposure_time_mode e
  ON e.c_observation_id = o.c_observation_id
 AND e.c_role = 'requirement'
ORDER BY
  o.c_observation_id,
  t.c_target_id;