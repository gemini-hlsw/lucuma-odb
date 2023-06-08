CREATE DOMAIN d_step_id AS varchar
  CHECK (VALUE ~ '^s-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$');
COMMENT ON DOMAIN d_step_id IS 'UUID for steps';

CREATE TYPE e_step_type AS ENUM (
  'bias',
  'dark',
  'gcal',
  'science',
  'smart_gcal'
);

CREATE TYPE e_guide_state AS ENUM (
  'enabled',
  'disabled'
);

CREATE TABLE t_step (
  c_step_id        d_step_id        PRIMARY KEY,

  -- Link back to the observation table.
  c_observation_id d_observation_id NOT NULL,
  c_instrument     d_tag            NOT NULL,
  FOREIGN KEY (c_observation_id, c_instrument) REFERENCES t_observation (c_observation_id, c_instrument),

  -- Link to the corresponding visit (if any)
  c_visit_id       d_visit_id       NULL REFERENCES t_visit (c_visit_id),

  c_step_type      e_step_type      NOT NULL,
  UNIQUE (c_step_id, c_step_type),

  c_created        timestamp        NOT NULL DEFAULT now()

);

CREATE TABLE t_step_config_gcal (
  c_step_id       d_step_id        PRIMARY KEY,

  c_step_type     e_step_type      NOT NULL DEFAULT ('gcal'),
  CHECK (c_step_type = 'gcal'),

  FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step (c_step_id, c_step_type),

  -- Gcal Configuration Value
  c_gcal_continuum   d_tag        REFERENCES t_gcal_continuum(c_tag) ON DELETE CASCADE,
  c_gcal_ar_arc      boolean      NOT NULL DEFAULT FALSE,
  c_gcal_cuar_arc    boolean      NOT NULL DEFAULT FALSE,
  c_gcal_thar_arc    boolean      NOT NULL DEFAULT FALSE,
  c_gcal_xe_arc      boolean      NOT NULL DEFAULT FALSE,

  -- NOTE: Either a continuum lamp or else one of the arcs, but never both.
  CONSTRAINT check_lamp CHECK ((c_gcal_continuum IS NULL) = (c_gcal_ar_arc OR c_gcal_cuar_arc OR c_gcal_thar_arc OR c_gcal_xe_arc)),

  c_gcal_filter      d_tag        NOT NULL REFERENCES t_gcal_filter(c_tag)   ON DELETE CASCADE,
  c_gcal_diffuser    d_tag        NOT NULL REFERENCES t_gcal_diffuser(c_tag) ON DELETE CASCADE,
  c_gcal_shutter     d_tag        NOT NULL REFERENCES t_gcal_shutter(c_tag)  ON DELETE CASCADE

);

CREATE TABLE t_step_config_science (
  c_step_id     d_step_id     PRIMARY KEY,

  c_step_type   e_step_type   NOT NULL DEFAULT ('science'),
  CHECK (c_step_type = 'science'),

  FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step (c_step_id, c_step_type),

  c_offset_p    d_angle_µas   NOT NULL DEFAULT (0),
  c_offset_q    d_angle_µas   NOT NULL DEFAULT (0),
  c_guide_state e_guide_state NOT NULL DEFAULT ('enabled')

);

CREATE TYPE e_smart_type AS ENUM (
  'arc',
  'flat',
  'day_baseline',
  'night_baseline'
);

CREATE TABLE t_step_config_smart_gcal (
  c_step_id     d_step_id   PRIMARY KEY,

  c_step_type   e_step_type NOT NULL DEFAULT ('smart_gcal'),
  CHECK (c_step_type = 'smart_gcal'),

  FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step (c_step_id, c_step_type),

  c_smart_type e_smart_type NOT NULL
);