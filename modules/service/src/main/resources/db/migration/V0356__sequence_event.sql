CREATE DOMAIN d_execution_event_id AS VARCHAR
  CHECK (VALUE ~ '^e-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$');

COMMENT ON DOMAIN d_execution_event_id IS 'UUID for execution events';

CREATE TYPE e_sequence_command AS ENUM(
  'abort',
  'continue',
  'pause',
  'slew',
  'start',
  'stop'
);

CREATE TABLE t_sequence_event (
  c_execution_event_id d_execution_event_id PRIMARY KEY,

  c_visit_id           d_visit_id           NOT NULL REFERENCES t_visit (c_visit_id),

  c_received           timestamp            NOT NULL DEFAULT now(),
  c_sequence_command   e_sequence_command   NOT NULL
);

CREATE INDEX sequence_event_visit_index ON t_sequence_event (c_visit_id);