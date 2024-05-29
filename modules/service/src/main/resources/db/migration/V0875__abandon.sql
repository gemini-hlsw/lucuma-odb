-- Drop the view since we're updating one of the columns.
DROP VIEW v_step_record;

-- Creates an execution state table.  Terminal states don't advance when new
-- events arrive.  We need a table to store this information so we'll convert
-- the enum to a table.
CREATE TABLE t_step_execution_state(
  c_tag      d_tag   PRIMARY KEY,
  c_name     text    NOT NULL,
  c_terminal boolean NOT NULL
);

INSERT INTO t_step_execution_state VALUES ('not_started', 'Not Started', false);
INSERT INTO t_step_execution_state VALUES ('ongoing',     'Ongoing',     false);
INSERT INTO t_step_execution_state VALUES ('aborted',     'Aborted',     true);
INSERT INTO t_step_execution_state VALUES ('completed',   'Completed',   true);
INSERT INTO t_step_execution_state VALUES ('stopped',     'Stopped',     true);
INSERT INTO t_step_execution_state VALUES ('abandoned',   'Abandoned',   true);

-- Update t_step_record to use a FK reference instead of an enum value.
ALTER TABLE t_step_record
  ALTER COLUMN c_execution_state DROP DEFAULT,
  ALTER COLUMN c_execution_state TYPE d_tag USING c_execution_state::d_tag,
  ADD CONSTRAINT t_step_record_c_step_execution_state_c_tag_fkey
    FOREIGN KEY (c_execution_state) REFERENCES t_step_execution_state(c_tag),
  ALTER COLUMN c_execution_state SET DEFAULT 'not_started'::d_tag;

-- No longer need the enumeration
DROP TYPE e_step_execution_state;

-- Recreate the view.
CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_step_index,
  s.c_atom_id,
  s.c_instrument,
  s.c_step_type,
  s.c_observe_class,
  s.c_created,
  s.c_execution_state,
  s.c_time_estimate,
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


-- Do the same for the atom execution state.  Fortunately there's no view to
-- update in this case.
CREATE TABLE t_atom_execution_state(
  c_tag      d_tag   PRIMARY KEY,
  c_name     text    NOT NULL,
  c_terminal boolean NOT NULL
);

INSERT INTO t_atom_execution_state VALUES ('not_started', 'Not Started', false);
INSERT INTO t_atom_execution_state VALUES ('ongoing',     'Ongoing',     false);
INSERT INTO t_atom_execution_state VALUES ('completed',   'Completed',   true);
INSERT INTO t_atom_execution_state VALUES ('abandoned',   'Abandoned',   true);

ALTER TABLE t_atom_record
  ALTER COLUMN c_execution_state DROP DEFAULT,
  ALTER COLUMN c_execution_state TYPE d_tag USING c_execution_state::d_tag,
  ADD CONSTRAINT t_atom_record_c_step_execution_state_c_tag_fkey
    FOREIGN KEY (c_execution_state) REFERENCES t_atom_execution_state(c_tag),
  ALTER COLUMN c_execution_state SET DEFAULT 'not_started'::d_tag;

DROP TYPE e_atom_execution_state;
