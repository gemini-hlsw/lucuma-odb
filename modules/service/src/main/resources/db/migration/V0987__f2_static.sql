-- Flamingos 2 Static Table

CREATE TABLE t_flamingos_2_static (
  c_static_id      bigserial        PRIMARY KEY,

  c_observation_id d_observation_id NOT NULL REFERENCES t_observation (c_observation_id),
  c_instrument     d_tag            NOT NULL DEFAULT ('Flamingos2'),
  FOREIGN KEY (c_observation_id, c_instrument)
  REFERENCES t_observation (c_observation_id, c_instrument),
  CHECK (c_instrument = 'Flamingos2'),

  c_visit_id       d_visit_id       NULL     REFERENCES t_visit (c_visit_id),
  UNIQUE NULLS NOT DISTINCT (c_observation_id, c_visit_id),

  c_mos_pre_imaging boolean         NOT NULL,
  c_use_eoffsetting boolean         NOT NULL
);