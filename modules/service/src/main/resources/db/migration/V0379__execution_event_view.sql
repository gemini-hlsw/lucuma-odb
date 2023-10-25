CREATE TYPE e_execution_event_type AS ENUM(
  'sequence',
  'step',
  'dataset'
);

-- Combine the sequence, step and dataset event tables into one view.
-- We do this to make GraphQL queries that return all events easy to implement.
CREATE VIEW v_execution_event AS
  (SELECT -- sequence event

    e.c_execution_event_id,
    'sequence' :: e_execution_event_type AS c_event_type,
    e.c_received,

    -- Foreign Keys
    v.c_observation_id,
    e.c_visit_id,
    NULL :: d_step_id    AS c_step_id,
    NULL :: d_dataset_id AS c_dataset_id,

    -- Data / Payload
    e.c_sequence_command,
    NULL :: e_step_stage    AS c_step_stage,
    NULL :: e_dataset_stage AS c_dataset_stage
  FROM
    t_sequence_event e
  LEFT JOIN t_visit v ON e.c_visit_id = v.c_visit_id)

  UNION ALL (SELECT -- step event

    e.c_execution_event_id,
    'step' :: e_execution_event_type AS c_event_type,
    e.c_received,

    -- Foreign Keys
    v.c_observation_id,
    a.c_visit_id,
    e.c_step_id,
    NULL :: d_dataset_id AS c_dataset_id,

    -- Data / Payload
    NULL :: e_sequence_command AS c_sequence_command,
    e.c_step_stage,
    NULL :: e_dataset_stage    AS c_daaset_stage
  FROM
    t_step_event e
  LEFT JOIN t_step_record s ON e.c_step_id  = s.c_step_id
  LEFT JOIN t_atom_record a ON s.c_atom_id  = a.c_atom_id
  LEFT JOIN t_visit       v ON a.c_visit_id = v.c_visit_id)


  UNION ALL (SELECT -- dataset event

    e.c_execution_event_id,
    'dataset' :: e_execution_event_type AS c_event_type,
    e.c_received,

    -- Foreign Keys
    d.c_observation_id,
    a.c_visit_id,
    s.c_step_id,
    e.c_dataset_id,

    -- Data / Payload
    NULL :: e_sequence_command AS c_sequence_command,
    NULL :: e_step_stage       AS c_step_stage,
    e.c_dataset_stage
  FROM
    t_dataset_event e
  LEFT JOIN t_dataset d     ON e.c_dataset_id = d.c_dataset_id
  LEFT JOIN t_step_record s ON d.c_step_id    = s.c_step_id
  LEFT JOIN t_atom_record a ON s.c_atom_id    = a.c_atom_id)

ORDER BY
  c_execution_event_id,
  c_observation_id,
  c_visit_id,
  c_step_id,
  c_dataset_id;