-- Split each sequence digest's time into arcs, flats and observing time (all
-- other steps), so observingTime + arcs + flats == timeEstimate per charge class.

-- Obscalc: nullable like the other digest columns (null when there is no digest).
ALTER TABLE t_obscalc
  ADD COLUMN c_acq_arc_count                  int4     NULL CHECK (c_acq_arc_count >= 0),
  ADD COLUMN c_acq_arc_non_charged_time       interval NULL CHECK (c_acq_arc_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_acq_arc_program_time           interval NULL CHECK (c_acq_arc_program_time >= interval '0 seconds'),
  ADD COLUMN c_acq_flat_count                 int4     NULL CHECK (c_acq_flat_count >= 0),
  ADD COLUMN c_acq_flat_non_charged_time      interval NULL CHECK (c_acq_flat_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_acq_flat_program_time          interval NULL CHECK (c_acq_flat_program_time >= interval '0 seconds'),
  ADD COLUMN c_acq_observing_non_charged_time interval NULL CHECK (c_acq_observing_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_acq_observing_program_time     interval NULL CHECK (c_acq_observing_program_time >= interval '0 seconds'),
  ADD COLUMN c_sci_arc_count                  int4     NULL CHECK (c_sci_arc_count >= 0),
  ADD COLUMN c_sci_arc_non_charged_time       interval NULL CHECK (c_sci_arc_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_sci_arc_program_time           interval NULL CHECK (c_sci_arc_program_time >= interval '0 seconds'),
  ADD COLUMN c_sci_flat_count                 int4     NULL CHECK (c_sci_flat_count >= 0),
  ADD COLUMN c_sci_flat_non_charged_time      interval NULL CHECK (c_sci_flat_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_sci_flat_program_time          interval NULL CHECK (c_sci_flat_program_time >= interval '0 seconds'),
  ADD COLUMN c_sci_observing_non_charged_time interval NULL CHECK (c_sci_observing_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_sci_observing_program_time     interval NULL CHECK (c_sci_observing_program_time >= interval '0 seconds');

-- Existing digests have no GCAL breakdown.  Treat all of their time as observing
-- time (keeping the sum invariant) until they are recomputed.
UPDATE t_obscalc
   SET c_acq_arc_count                  = 0,
       c_acq_arc_non_charged_time       = interval '0 seconds',
       c_acq_arc_program_time           = interval '0 seconds',
       c_acq_flat_count                 = 0,
       c_acq_flat_non_charged_time      = interval '0 seconds',
       c_acq_flat_program_time          = interval '0 seconds',
       c_acq_observing_non_charged_time = c_acq_non_charged_time,
       c_acq_observing_program_time     = c_acq_program_time,
       c_sci_arc_count                  = 0,
       c_sci_arc_non_charged_time       = interval '0 seconds',
       c_sci_arc_program_time           = interval '0 seconds',
       c_sci_flat_count                 = 0,
       c_sci_flat_non_charged_time      = interval '0 seconds',
       c_sci_flat_program_time          = interval '0 seconds',
       c_sci_observing_non_charged_time = c_sci_non_charged_time,
       c_sci_observing_program_time     = c_sci_program_time
 WHERE c_setup_count IS NOT NULL;

UPDATE t_obscalc SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
WHERE c_obscalc_state IN ('ready', 'retry');

TRUNCATE TABLE t_execution_digest;

ALTER TABLE t_execution_digest
  ADD COLUMN c_acq_arc_count                  int4     NOT NULL CHECK (c_acq_arc_count >= 0),
  ADD COLUMN c_acq_arc_non_charged_time       interval NOT NULL CHECK (c_acq_arc_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_acq_arc_program_time           interval NOT NULL CHECK (c_acq_arc_program_time >= interval '0 seconds'),
  ADD COLUMN c_acq_flat_count                 int4     NOT NULL CHECK (c_acq_flat_count >= 0),
  ADD COLUMN c_acq_flat_non_charged_time      interval NOT NULL CHECK (c_acq_flat_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_acq_flat_program_time          interval NOT NULL CHECK (c_acq_flat_program_time >= interval '0 seconds'),
  ADD COLUMN c_acq_observing_non_charged_time interval NOT NULL CHECK (c_acq_observing_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_acq_observing_program_time     interval NOT NULL CHECK (c_acq_observing_program_time >= interval '0 seconds'),
  ADD COLUMN c_sci_arc_count                  int4     NOT NULL CHECK (c_sci_arc_count >= 0),
  ADD COLUMN c_sci_arc_non_charged_time       interval NOT NULL CHECK (c_sci_arc_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_sci_arc_program_time           interval NOT NULL CHECK (c_sci_arc_program_time >= interval '0 seconds'),
  ADD COLUMN c_sci_flat_count                 int4     NOT NULL CHECK (c_sci_flat_count >= 0),
  ADD COLUMN c_sci_flat_non_charged_time      interval NOT NULL CHECK (c_sci_flat_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_sci_flat_program_time          interval NOT NULL CHECK (c_sci_flat_program_time >= interval '0 seconds'),
  ADD COLUMN c_sci_observing_non_charged_time interval NOT NULL CHECK (c_sci_observing_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_sci_observing_program_time     interval NOT NULL CHECK (c_sci_observing_program_time >= interval '0 seconds');
