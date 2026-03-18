
CREATE TABLE t_igrins_2_static (
  c_static_id       bigserial                 PRIMARY KEY,
  c_observation_id  d_observation_id NOT NULL REFERENCES t_observation (c_observation_id),
  c_instrument      d_tag            NOT NULL DEFAULT ('Igrins2'),

  FOREIGN KEY (c_observation_id, c_instrument)
  REFERENCES t_observation (c_observation_id, c_instrument),

  CHECK (c_instrument = 'Igrins2'),

  c_visit_id        d_visit_id       NULL     REFERENCES t_visit (c_visit_id),

  UNIQUE NULLS NOT DISTINCT (c_observation_id, c_visit_id),
  c_save_svc_images boolean          NOT NULL,
  c_offset_mode     d_tag            NOT NULL
);

CREATE TABLE t_igrins_2_dynamic (
  c_step_id        d_step_id              PRIMARY KEY,
  c_instrument     d_tag        NOT NULL DEFAULT ('Igrins2'),

  FOREIGN KEY (c_step_id, c_instrument)
  REFERENCES t_step_record (c_step_id, c_instrument),

  CHECK (c_instrument = 'Igrins2'),
  c_exposure_time  interval     NOT NULL
);
