-- Drop the program id from the execution digest.
ALTER TABLE t_execution_digest
  DROP COLUMN c_program_id;

-- Add an index on observation id
CREATE INDEX execution_digest_observation_id_index ON t_execution_digest (c_observation_id);

CREATE TABLE t_atom (
  c_atom_id         d_atom_id        PRIMARY KEY,
  c_instrument      d_tag            NOT NULL,
  c_atom_index      integer          NOT NULL,

  UNIQUE (c_atom_id, c_instrument),

  c_observation_id  d_observation_id NOT NULL,
  c_sequence_type   e_sequence_type  NOT NULL,
  c_visit_id        d_visit_id       REFERENCES t_visit(c_visit_id) ON DELETE CASCADE,
  c_execution_state d_tag            NOT NULL REFERENCES t_atom_execution_state(c_tag) DEFAULT ('not_started'::character varying)::d_tag,

  c_description     text
);

ALTER TABLE ONLY t_atom
  ADD CONSTRAINT t_atom_c_observation_id_c_instrument_fkey FOREIGN KEY (c_observation_id, c_instrument) REFERENCES t_observation(c_observation_id, c_instrument) ON DELETE CASCADE;

CREATE INDEX atom_observation_id_exeuction_stateindex ON t_atom (c_observation_id, c_execution_state);


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

  c_observe_class    e_obs_class   NOT NULL,
  c_time_estimate    interval      NOT NULL DEFAULT '00:00:00'::interval,
  c_execution_state  d_tag         NOT NULL REFERENCES t_step_execution_state(c_tag) DEFAULT ('not_started'::character varying)::d_tag,
  c_offset_p         d_angle_µas   NOT NULL DEFAULT 0,
  c_offset_q         d_angle_µas   NOT NULL DEFAULT 0,
  c_guide_state      e_guide_state NOT NULL DEFAULT 'enabled'::e_guide_state,

  c_breakpoint       e_breakpoint  NOT NULL DEFAULT 'disabled'::e_breakpoint,

  c_completed        timestamp without time zone,
  c_first_event_time timestamp without time zone,
  c_last_event_time  timestamp without time zone
);

ALTER TABLE ONLY t_step
  ADD CONSTRAINT t_atom_c_atom_id_c_instrument_fkey FOREIGN KEY (c_atom_id, c_instrument) REFERENCES t_atom(c_atom_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

CREATE TRIGGER step_obscalc_invalidate_trigger
  AFTER UPDATE ON t_step FOR EACH ROW WHEN ((num_nulls(new.c_completed, old.c_completed) = 1)) EXECUTE FUNCTION step_record_invalidate();

-- TEMPORARILY relax the FK constraints for these tables while t_step_record
-- continues to exist.  We will have entries referring to steps in t_step_record
-- and entries referring to t_step.

ALTER TABLE t_step_config_gcal
  DROP CONSTRAINT t_step_config_gcal_c_step_id_c_step_type_fkey;

ALTER TABLE t_step_config_smart_gcal
  DROP CONSTRAINT t_step_config_smart_gcal_c_step_id_c_step_type_fkey;

ALTER TABLE t_gmos_north_dynamic
  DROP CONSTRAINT t_gmos_north_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_gmos_south_dynamic
  DROP CONSTRAINT t_gmos_south_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_flamingos_2_dynamic
  DROP CONSTRAINT t_flamingos_2_dynamic_c_step_id_c_instrument_fkey;

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

-- Sequence serialization coordination

CREATE TABLE t_sequence_materialization (
  c_observation_id d_observation_id PRIMARY KEY,
  c_created_at     timestamp        NOT NULL,
  c_updated_at     timestamp        NOT NULL
);
