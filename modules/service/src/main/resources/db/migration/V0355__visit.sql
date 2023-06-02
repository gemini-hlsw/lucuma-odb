CREATE DOMAIN d_visit_id AS varchar
 CHECK (VALUE ~ '^v-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$');
 COMMENT ON DOMAIN d_visit_id IS 'UUID for observation visits';

CREATE TABLE t_visit (
  c_visit_id       d_visit_id       PRIMARY KEY,

  -- Link back to the observation table.
  c_observation_id d_observation_id NOT NULL,
  c_instrument     d_tag            NOT NULL,
  FOREIGN KEY (c_observation_id, c_instrument) REFERENCES t_observation (c_observation_id, c_instrument),

  c_created        timestamp        NOT NULL DEFAULT now()
);

create table t_gmos_south_static (
  c_static_id       bigserial        PRIMARY KEY,

  -- Link back to the observation table.
  c_observation_id  d_observation_id NOT NULL,
  c_instrument      d_tag            NOT NULL DEFAULT ('GmosSouth'),
  FOREIGN KEY (c_observation_id, c_instrument)
  REFERENCES t_observation (c_observation_id, c_instrument),
  CHECK (c_instrument = 'GmosSouth'),

  -- Link to the corresponding visit (if any). There can only be at most one
  -- entry with a null visit id (per observation).  This corresponds to a manual
  -- sequence entry.
  c_visit_id        d_visit_id      NULL REFERENCES t_visit (c_visit_id),
  UNIQUE NULLS NOT DISTINCT (c_observation_id, c_visit_id),

  -- GMOS South static instrument properties
  c_detector        d_tag           NOT NULL REFERENCES t_gmos_south_detector(c_tag),
  c_mos_pre_imaging boolean         NOT NULL,
  c_nod_and_shuffle bigint          NULL REFERENCES t_gmos_nod_and_shuffle(c_gmos_nod_and_shuffle_id),
  c_stage_mode      d_tag           NOT NULL REFERENCES t_gmos_south_stage_mode(c_tag)
);

-- Alter GMOS North Static to match
ALTER TABLE t_gmos_north_static
   DROP CONSTRAINT t_gmos_north_static_pkey,
   ADD COLUMN      c_static_id bigserial  PRIMARY KEY,
   ADD COLUMN      c_visit_id  d_visit_id NULL REFERENCES t_visit (c_visit_id),
   ADD UNIQUE NULLS NOT DISTINCT (c_observation_id, c_visit_id);