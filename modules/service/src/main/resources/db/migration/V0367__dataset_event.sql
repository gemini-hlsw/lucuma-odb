-- Dataset execution stages
CREATE TYPE e_dataset_stage AS ENUM(
  'end_observe',
  'end_readout',
  'end_write',
  'start_observe',
  'start_readout',
  'start_write'
);

-- Dataset events
CREATE TABLE t_dataset_event (
  c_execution_event_id d_execution_event_id PRIMARY KEY DEFAULT 'e-' || to_hex(nextval('s_execution_event_id')),
  c_visit_id           d_visit_id           NOT NULL REFERENCES t_visit (c_visit_id),
  c_step_id            d_step_id            NOT NULL REFERENCES t_step (c_step_id),
  c_index              int2                 NOT NULL CHECK (c_index >= 0),
  c_received           timestamp            NOT NULL DEFAULT now(),

  c_dataset_stage      e_dataset_stage      NOT NULL,

  c_file_site          e_site               NULL,
  c_file_date          date                 NULL CHECK (EXTRACT(YEAR FROM c_file_date) BETWEEN 0 AND 9999),
  c_file_index         int4                 NULL CHECK (c_file_index >= 1),
  CHECK (
    ( c_file_site  IS NULL AND
      c_file_date  IS NULL AND
      c_file_index IS NULL
    ) OR (
      c_file_site  IS NOT NULL AND
      c_file_date  IS NOT NULL AND
      c_file_index IS NOT NULL
    )
  )

);

CREATE INDEX dataset_event_visit_index ON t_dataset_event (c_visit_id);