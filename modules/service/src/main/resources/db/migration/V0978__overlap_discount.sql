-- A new time charge discount, "overlap" time.  This is time that is not going
-- to be charged because we switched to some other chargeable visit.  For
-- example, time spent writing out the last datasets while slewing to another
-- observation.
ALTER TYPE e_time_charge_discount_type
  ADD VALUE 'overlap' AFTER 'nodata';

CREATE TABLE t_time_charge_discount_overlap (
  c_discount_id    int8             NOT NULL REFERENCES t_time_charge_discount(c_id)    ON DELETE CASCADE,
  c_observation_id d_observation_id NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,

  PRIMARY KEY (c_discount_id)
);

-- To keep up with overlaps, we really need a convenient way to get the start
-- and end time of each visit.  We'll add these to the visit table.
ALTER TABLE t_visit
  ADD COLUMN c_start timestamp NULL,
  ADD COLUMN c_end   timestamp NULL;

-- Initialize the visit times for existing visits.
UPDATE t_visit v
   SET c_start = sub.min_ts,
       c_end   = sub.max_ts
  FROM (
    SELECT
      c_visit_id,
      min(c_received) AS min_ts,
      max(c_received) AS max_ts
    FROM
      t_execution_event
    GROUP BY
      c_visit_id
  ) sub
 WHERE v.c_visit_id = sub.c_visit_id;

-- Add an index to make the overlap query faster.
CREATE INDEX idx_t_visit_site_chargeable_start
  ON t_visit (c_site, c_chargeable, c_start);

-- Resets the visit times for a particular visit when a new event recorded.
-- (Also when updated or deleted but we don't really do that.)
CREATE OR REPLACE FUNCTION update_visit_times()
RETURNS TRIGGER AS $$
DECLARE
  visit_id d_visit_id := COALESCE(NEW.c_visit_id, OLD.c_visit_id);
  visit_min timestamp;
  visit_max timestamp;
BEGIN
  SELECT
    min(c_received),
    max(c_received)
  INTO visit_min, visit_max
  FROM t_execution_event e
  WHERE c_visit_id = visit_id;

  UPDATE t_visit
  SET c_start = visit_min,
      c_end   = visit_max
  WHERE c_visit_id = visit_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_visit_times_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_execution_event
FOR EACH ROW
EXECUTE FUNCTION update_visit_times();