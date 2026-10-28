-- Visitor instruments are not executed with Observe, so their visits have only
-- the slew event(s) recorded by Navigate.  Time accounting charges them from
-- the slew until the next chargeable visit at the same site starts or, once the
-- night is over, until nautical dawn, whichever comes first.  A visitor visit's
-- charge therefore depends on other visits and on the time of day.
--
-- Rather than invalidating the visitor visit from the transaction that starts
-- the next visit (which would lock a second observation inside an event insert
-- and can deadlock), time accounting records the end that it used and the
-- obscalc worker periodically invalidates recent visitor visits whose end
-- would now be different.

ALTER TABLE t_visit
  ADD COLUMN c_ta_visitor_end timestamp NULL;

COMMENT ON COLUMN t_visit.c_ta_visitor_end IS
  'For visitor instrument visits, the end of the charged interval as of the last time accounting update, or NULL if it was not extended past the last event.';

-- Lets the worker's sweep pick out recent visitor visits.  (The next chargeable
-- visit at a site is found with idx_t_visit_site_chargeable_start from V0978.)
CREATE INDEX i_visit_instrument_start
  ON t_visit (c_instrument, c_start);

-- Reprice existing visitor visits under the new rule.  Marking them dirty and
-- invalidating their observations lets the obscalc worker recompute them.
--
-- Observations in which staff have corrected the time charge of any visit are
-- left alone.  Visitor visits were charged nothing under the old rule, so a
-- correction there most likely adds the time by hand, and repricing would
-- count it twice.  They are old enough that the worker's sweep never looks at
-- them either.
CREATE TEMPORARY TABLE visitor_visit_reprice AS
  SELECT
    v.c_visit_id,
    v.c_observation_id
  FROM t_visit v
  INNER JOIN t_instrument i ON i.c_tag = v.c_instrument
  WHERE i.c_is_visitor
    AND v.c_start IS NOT NULL
    AND NOT EXISTS (
      SELECT 1
        FROM t_time_charge_correction c
        INNER JOIN t_visit cv ON cv.c_visit_id = c.c_visit_id
       WHERE cv.c_observation_id = v.c_observation_id
    );

UPDATE t_visit v
   SET c_ta_invalidation = now()
  FROM visitor_visit_reprice r
 WHERE r.c_visit_id = v.c_visit_id;

DO $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  SELECT array_agg(DISTINCT c_observation_id) INTO obs_ids
    FROM visitor_visit_reprice;

  IF obs_ids IS NOT NULL THEN
    CALL invalidate_obscalc_many(obs_ids);
  END IF;
END;
$$;

DROP TABLE visitor_visit_reprice;
