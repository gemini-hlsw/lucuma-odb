
-- Table of anticipated atoms
CREATE TABLE t_atom_digest (
  c_atom_id                    d_atom_id          NOT NULL PRIMARY KEY,
  c_atom_index                 smallint           NOT NULL,
  UNIQUE (c_atom_id, c_atom_index),

  c_observation_id             d_observation_id   NOT NULL REFERENCES t_observation (c_observation_id) ON DELETE CASCADE,

  c_observe_class              e_obs_class        NOT NULL DEFAULT 'science' :: e_obs_class,
  c_non_charged_time_estimate  interval           NOT NULL DEFAULT '0'::interval,
  c_program_time_estimate      interval           NOT NULL DEFAULT '0'::interval,
  c_step_types                 e_step_type[]      NOT NULL DEFAULT '{}',
  c_lamp_types                 e_gcal_lamp_type[] NOT NULL DEFAULT '{}'

);

CREATE INDEX atom_digest_observation_index ON t_atom_digest (c_observation_id);