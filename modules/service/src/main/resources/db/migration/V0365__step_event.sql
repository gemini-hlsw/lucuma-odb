
-- Sequence type
CREATE TYPE e_sequence_type AS ENUM(
  'acquisition',
  'science'
);

-- Step execution stages
CREATE TYPE e_step_stage AS ENUM(
  'end_configure',
  'end_observe',
  'end_step',
  'start_configure',
  'start_observe',
  'start_step'
);

-- Step events
CREATE TABLE t_step_event (
  c_execution_event_id d_execution_event_id PRIMARY KEY DEFAULT 'e-' || to_hex(nextval('s_execution_event_id')),
  c_visit_id           d_visit_id           NOT NULL REFERENCES t_visit (c_visit_id),
  c_step_id            d_step_id            NOT NULL REFERENCES t_step (c_step_id),
  c_received           timestamp            NOT NULL DEFAULT now(),

  c_sequence_type      e_sequence_type      NOT NULL,
  c_step_stage         e_step_stage         NOT NULL
);

CREATE INDEX step_event_visit_index ON t_step_event (c_visit_id);