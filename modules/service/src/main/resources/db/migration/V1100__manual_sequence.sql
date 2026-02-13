-- Drop the program id from the execution digest.
ALTER TABLE t_execution_digest
  DROP COLUMN c_program_id;

-- Add an index on observation id
CREATE INDEX execution_digest_observation_id_index ON t_execution_digest (c_observation_id);

CREATE TABLE t_atom (
  c_atom_id          d_atom_id        PRIMARY KEY,
  c_instrument       d_tag            NOT NULL,
  c_atom_index       integer          NOT NULL,

  UNIQUE (c_atom_id, c_instrument),

  c_observation_id   d_observation_id NOT NULL,

  UNIQUE (c_observation_id, c_atom_index),

  c_sequence_type    e_sequence_type  NOT NULL,
  c_visit_id         d_visit_id       REFERENCES t_visit(c_visit_id) ON DELETE CASCADE,

  c_description      text,

  c_execution_state  d_tag            NOT NULL REFERENCES t_atom_execution_state(c_tag) DEFAULT ('not_started'::character varying)::d_tag,
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
  c_execution_state,
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
  r2.c_execution_state,
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
    ) - 1 AS c_atom_index,
    r.c_sequence_type,
    r.c_visit_id,
    r.c_execution_state
  FROM t_atom_record r
) r2 -- for atom index calculation
LEFT JOIN t_execution_event e ON e.c_atom_id = r2.c_atom_id
GROUP BY -- for MIN/MAX aggregation
  r2.c_atom_id,
  r2.c_instrument,
  r2.c_atom_index,
  r2.c_observation_id,
  r2.c_sequence_type,
  r2.c_visit_id,
  r2.c_execution_state;

ALTER TABLE ONLY t_atom
  ADD CONSTRAINT t_atom_c_observation_id_c_instrument_fkey FOREIGN KEY (c_observation_id, c_instrument) REFERENCES t_observation(c_observation_id, c_instrument) ON DELETE CASCADE;

CREATE INDEX atom_observation_id_exeuction_state_index ON t_atom (c_observation_id, c_execution_state);

CREATE TYPE e_breakpoint AS enum(
  'enabled',
  'disabled'
);

CREATE TABLE t_step (
  c_step_id          d_step_id     PRIMARY KEY,
  c_atom_id          d_atom_id     NOT NULL REFERENCES t_atom(c_atom_id) DEFERRABLE INITIALLY DEFERRED,
  c_instrument       d_tag         NOT NULL,
  c_step_type        e_step_type   NOT NULL,
  c_step_index       integer       NOT NULL,

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
CREATE TRIGGER step_obscalc_invalidate_trigger
AFTER UPDATE ON t_step FOR EACH ROW WHEN (
  OLD.c_execution_state IS DISTINCT FROM NEW.c_execution_state AND (
    OLD.c_execution_state = 'completed' OR
    NEW.c_execution_state = 'completed'
  )
) EXECUTE FUNCTION step_record_invalidate();

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
  ADD CONSTRAINT t_step_config_gcal_c_step_id_c_step_type_fkey FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step(c_step_id, c_step_type);

ALTER TABLE t_step_config_smart_gcal
  DROP CONSTRAINT t_step_config_smart_gcal_c_step_id_c_step_type_fkey;

ALTER TABLE t_step_config_smart_gcal
  ADD CONSTRAINT t_step_config_smart_gcal_c_step_id_c_step_type_fkey FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step(c_step_id, c_step_type);

ALTER TABLE t_flamingos_2_dynamic
  DROP CONSTRAINT t_flamingos_2_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_flamingos_2_dynamic
  ADD CONSTRAINT t_flamingos_2_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument);

ALTER TABLE t_gmos_north_dynamic
  DROP CONSTRAINT t_gmos_north_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_gmos_north_dynamic
  ADD CONSTRAINT t_gmos_north_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument);

ALTER TABLE t_gmos_south_dynamic
  DROP CONSTRAINT t_gmos_south_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_gmos_south_dynamic
  ADD CONSTRAINT t_gmos_south_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument);

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
      SELECT 1 FROM t_atom a
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