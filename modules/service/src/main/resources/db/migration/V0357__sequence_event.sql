-- Execution Event ID type
CREATE DOMAIN d_execution_event_id AS VARCHAR
  CHECK (VALUE ~ '^e-[1-9a-f][0-9a-f]*$');

COMMENT ON DOMAIN d_execution_event_id IS 'ID for execution events';

CREATE SEQUENCE s_execution_event_id START WITH 256;

-- Sequence commands
CREATE TYPE e_sequence_command AS ENUM(
  'abort',
  'continue',
  'pause',
  'slew',
  'start',
  'stop'
);

-- Sequence events
CREATE TABLE t_sequence_event (
  c_execution_event_id d_execution_event_id PRIMARY KEY DEFAULT 'e-' || to_hex(nextval('s_execution_event_id')),

  c_visit_id           d_visit_id           NOT NULL REFERENCES t_visit (c_visit_id),

  c_received           timestamp            NOT NULL DEFAULT now(),
  c_sequence_command   e_sequence_command   NOT NULL
);

CREATE INDEX sequence_event_visit_index ON t_sequence_event (c_visit_id);