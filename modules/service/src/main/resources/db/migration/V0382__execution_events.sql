

-- Create an execution event table that matches the v_execution_event view
-- which combines the three event tables: t_sequence_event, t_step_event
-- and t_sequence_event.

CREATE TABLE t_execution_event (
  c_execution_event_id d_execution_event_id   PRIMARY KEY DEFAULT 'e-' || to_hex(nextval('s_execution_event_id')),
  c_event_type         e_execution_event_type NOT NULL,
  c_received           TIMESTAMP              NOT NULL DEFAULT now(),

  -- FK references.  All events are associated with an obsevation and visit.
  -- Step and dataset events have an associated step.  Dataset events refer to
  -- a particular dataset.

  c_observation_id     d_observation_id       NOT NULL REFERENCES t_observation (c_observation_id),
  c_visit_id           d_visit_id             NOT NULL REFERENCES t_visit (c_visit_id),
  c_step_id            d_step_id              NULL     REFERENCES t_step_record (c_step_id),
  c_dataset_id         d_dataset_id           NULL     REFERENCES t_dataset (c_dataset_id),

  c_sequence_command   e_sequence_command     NULL,
  c_step_stage         e_step_stage           NULL,
  c_dataset_stage      e_dataset_stage        NULL,

  CONSTRAINT check_event_type_conditions CHECK (
    CASE
      WHEN c_event_type = 'sequence' THEN c_sequence_command IS NOT NULL AND c_step_ID IS NULL     AND c_dataset_id IS NULL
      WHEN c_event_type = 'step'     THEN c_step_stage       IS NOT NULL AND c_step_id IS NOT NULL AND c_dataset_id IS NULL
      WHEN c_event_type = 'dataset'  THEN c_dataset_stage    IS NOT NULL AND c_step_id IS NOT NULL AND c_dataset_id IS NOT NULL
      ELSE FALSE
    END
  )

);

-- Copy the data from the view into the table.
INSERT INTO t_execution_event
  SELECT * FROM v_execution_event;

-- Delete the view and the 3 tables it combines.
DROP VIEW v_execution_event;
DROP TABLE t_dataset_event;
DROP TABLE t_step_event;
DROP TABLE t_sequence_event;
