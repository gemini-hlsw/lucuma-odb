CREATE DOMAIN d_atom_id AS varchar
  CHECK (VALUE ~ '^a-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$');

COMMENT ON DOMAIN d_atom_id IS 'UUID for atoms';

CREATE TABLE t_atom_record (
  c_atom_id        d_atom_id        PRIMARY KEY,

  -- Link back to the observation table.
  c_observation_id d_observation_id NOT NULL,
  c_instrument     d_tag            NOT NULL,
  FOREIGN KEY (c_observation_id, c_instrument) REFERENCES t_observation (c_observation_id, c_instrument) ON DELETE CASCADE,

  -- Serves as a FK target from t_step.  This ensures that the step instrument
  -- matches the atom instrument.  The link to t_observation ensures that the
  -- atom instrument matches the observation instrument.
  UNIQUE (c_atom_id, c_instrument),

  c_visit_id       d_visit_id       NOT NULL REFERENCES t_visit (c_visit_id) ON DELETE CASCADE,

  -- How many steps are in this atom
  c_step_count     int2             NOT NULL CHECK (c_step_count >= 0),

  -- Type of sequence in which the atom is found
  c_sequence_type  e_sequence_type  NOT NULL,

  c_created        timestamp        NOT NULL DEFAULT now()
);

COMMENT ON COLUMN t_atom_record.c_step_count IS 'number of steps in the atom';

DROP VIEW v_step;

-- There are no significant executed or manual steps yet.  This makes it easier
-- to add NOT NULL columns.
TRUNCATE TABLE t_step CASCADE;

ALTER TABLE t_step
  DROP CONSTRAINT t_step_c_observation_id_c_instrument_fkey,
  DROP COLUMN c_observation_id,
  DROP COLUMN c_visit_id,
  ADD  COLUMN c_atom_id    d_atom_id NOT NULL,
  ADD  COLUMN c_step_index int2      NOT NULL CHECK (c_step_index >= 0),
  ADD  CONSTRAINT t_atom_record_c_atom_id_c_instrument_fkey
    FOREIGN KEY (c_atom_id, c_instrument)
    REFERENCES  t_atom_record (c_atom_id, c_instrument) ON DELETE CASCADE;

COMMENT ON COLUMN t_step.c_step_index IS 'index of step within its atom';

ALTER TABLE t_step
  RENAME TO t_step_record;

-- A view that ties together all the step config tables, primarily to simplify
-- mapping logic.
CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_atom_id,
  s.c_instrument,
  s.c_step_type,
  s.c_step_index,
  s.c_created,
  g.c_gcal_continuum,
  g.c_gcal_ar_arc,
  g.c_gcal_cuar_arc,
  g.c_gcal_thar_arc,
  g.c_gcal_xe_arc,
  g.c_gcal_filter,
  g.c_gcal_diffuser,
  g.c_gcal_shutter,
  n.c_offset_p,
  n.c_offset_q,
  n.c_guide_state,
  m.c_smart_gcal_type
FROM
  t_step_record s
LEFT JOIN t_step_config_gcal g
  ON g.c_step_id = s.c_step_id
LEFT JOIN t_step_config_science n
  ON n.c_step_id = s.c_step_id
LEFT JOIN t_step_config_smart_gcal m
  ON m.c_step_id = s.c_step_id
ORDER BY
  s.c_step_id;

TRUNCATE TABLE t_step_event;

ALTER TABLE t_step_event
  DROP COLUMN c_visit_id,
  DROP COLUMN c_sequence_type;

TRUNCATE TABLE t_dataset_event;

ALTER TABLE t_dataset_event
  DROP COLUMN c_visit_id;