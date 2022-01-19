
-- OBSERVATIONS

CREATE DOMAIN d_observation_id AS VARCHAR; -- TODO format check
comment on domain d_observation_id is 'GID type for observations.';

CREATE SEQUENCE s_observation_id START WITH 256; -- three hex digits
CREATE TABLE t_observation (
  c_program_id     d_program_id     NOT NULL REFERENCES t_program(c_program_id),
  c_observation_id d_observation_id NOT NULL PRIMARY KEY DEFAULT 'o-' || to_hex(nextval('s_observation_id')),
  c_existence      e_existence      NOT NULL DEFAULT 'present',
  c_instrument     d_tag            NOT NULL REFERENCES t_instrument(c_tag),
  UNIQUE (c_observation_id, c_instrument)
);
comment on table t_observation is 'Observations.';

