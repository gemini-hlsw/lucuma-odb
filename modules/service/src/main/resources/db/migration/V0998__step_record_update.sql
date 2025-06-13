-- Add an updated timestamp to the step record table.
ALTER TABLE t_step_record
  ADD COLUMN c_first_event_time timestamp NULL,
  ADD COLUMN c_last_event_time  timestamp NULL;


-- Set the updated time for all the step records for which there were events.
UPDATE t_step_record sr
   SET c_first_event_time = e.first_received,
       c_last_event_time  = e.last_received
  FROM (
    SELECT c_step_id,
           MIN(c_received) AS first_received,
           MAX(c_received) AS last_received
      FROM t_execution_event
     WHERE c_step_id IS NOT NULL GROUP BY c_step_id
  ) e
WHERE sr.c_step_id = e.c_step_id;

DROP VIEW v_step_record;

-- Recreate the view to add first/last event time. Modified from V0971.
CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_atom_id,
  a.c_visit_id,
  s.c_step_index,
  s.c_instrument,
  s.c_step_type,
  s.c_observe_class,
  s.c_created,
  s.c_execution_state,
  s.c_time_estimate,
  s.c_generated_id,
  s.c_first_event_time,
  s.c_last_event_time,
  g.c_gcal_continuum,
  g.c_gcal_ar_arc,
  g.c_gcal_cuar_arc,
  g.c_gcal_thar_arc,
  g.c_gcal_xe_arc,
  g.c_gcal_filter,
  g.c_gcal_diffuser,
  g.c_gcal_shutter,
  m.c_smart_gcal_type,
  s.c_offset_p,
  s.c_offset_q,
  s.c_guide_state,
  (SELECT MAX(c_qa_state) FROM t_dataset d WHERE d.c_step_id = s.c_step_id) AS c_qa_state
FROM
  t_step_record s
LEFT JOIN t_step_config_gcal g
  ON g.c_step_id = s.c_step_id
LEFT JOIN t_step_config_smart_gcal m
  ON m.c_step_id = s.c_step_id
LEFT JOIN t_atom_record a
  ON a.c_atom_id = s.c_atom_id
ORDER BY
  s.c_step_id;

-- A trigger to keep the first/last event data accurate.
CREATE OR REPLACE FUNCTION update_step_record_event_times()
  RETURNS TRIGGER AS $$
BEGIN

  UPDATE t_step_record
     SET c_first_event_time = least(coalesce(c_first_event_time, NEW.c_received), NEW.c_received),
         c_last_event_time  = greatest(coalesce(c_last_event_time, NEW.c_received), NEW.c_received)
   WHERE c_step_id = NEW.c_step_id;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_step_record_event_times_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW
  WHEN (NEW.c_step_id IS NOT NULL)
    EXECUTE PROCEDURE update_step_record_event_times();