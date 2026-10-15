-- The setups found in each visit by time accounting, with the time charged to
-- each broken down into slewing, the acquisition sequence proper, and other
-- setup time.  Rows are rewritten whenever the visit's time accounting is
-- recomputed.

CREATE TYPE e_setup_kind AS ENUM(
  'full',
  'reacquisition'
);

CREATE TABLE t_visit_setup (
  c_visit_id                 d_visit_id   NOT NULL REFERENCES t_visit(c_visit_id) ON DELETE CASCADE,
  c_index                    integer      NOT NULL CHECK (c_index >= 0),
  c_kind                     e_setup_kind NOT NULL,

  c_start                    timestamp    NOT NULL,
  c_end                      timestamp    NOT NULL,

  c_slew_non_charged_time    interval     NOT NULL DEFAULT '0'::interval,
  c_slew_program_time        interval     NOT NULL DEFAULT '0'::interval,
  c_acq_non_charged_time     interval     NOT NULL DEFAULT '0'::interval,
  c_acq_program_time         interval     NOT NULL DEFAULT '0'::interval,
  c_other_non_charged_time   interval     NOT NULL DEFAULT '0'::interval,
  c_other_program_time       interval     NOT NULL DEFAULT '0'::interval,

  PRIMARY KEY (c_visit_id, c_index),
  CHECK (c_start <= c_end)
);

-- Record setups for existing visits by recomputing their time accounting.
-- Marking each visit that has execution events dirty (and invalidating its
-- observation) lets the obscalc worker recompute it through the normal path.
-- N.B. this is a full recompute, so these visits are also repriced with the
-- current algorithm.  That is acceptable only because there is little
-- production history yet; in general visits are deliberately never repriced
-- without a change of their own.
DO $$
DECLARE
  r record;
BEGIN
  FOR r IN
    SELECT v.c_visit_id, v.c_observation_id
      FROM t_visit v
     WHERE EXISTS (SELECT 1 FROM t_execution_event e WHERE e.c_visit_id = v.c_visit_id)
  LOOP
    CALL invalidate_visit_time_accounting(r.c_visit_id, r.c_observation_id);
  END LOOP;
END $$;
