-- Proposal summary PDF jobs.
--
-- A queue of (program, partner) pairs whose summary PDF needs rendering,
-- filled when a proposal is submitted or a regeneration is requested.  The
-- pdf-summary daemon claims a job, builds the renderer payload, renders the
-- PDF with pyexplore as a subprocess, uploads it, replaces the program's
-- SUMMARY attachment for that partner, and deletes the row.  Only the ODB
-- touches this table; the renderer sees the payload JSON.
--
-- State machine:
--   pending   -> rendering  (the daemon picked the job up)
--   rendering -> (deleted)  (PDF uploaded, attachment row replaced)
--   rendering -> pending    (transient failure, retried after c_retry_at;
--                            or a stale job whose daemon died mid-render)
--   rendering -> failed     (permanent failure, or the attempt cap)

CREATE TYPE e_summary_job_state AS ENUM(
  'pending',
  'rendering',
  'failed'
);

CREATE SEQUENCE s_summary_job_id START WITH 256;

CREATE TABLE t_summary_job (
  c_summary_job_id  int8                 PRIMARY KEY DEFAULT nextval('s_summary_job_id'),
  c_program_id      d_program_id         NOT NULL REFERENCES t_program(c_program_id) ON DELETE CASCADE,
  c_partner         d_tag                NULL     REFERENCES t_partner(c_tag),
  c_style           e_summary_style      NOT NULL,

  c_state           e_summary_job_state  NOT NULL DEFAULT 'pending',
  c_attempts        int4                 NOT NULL DEFAULT 0 CHECK (c_attempts >= 0),
  -- A pending job is not picked up before this.
  c_retry_at        timestamp            NULL,
  c_error           text                 NULL,

  c_created_at      timestamp            NOT NULL DEFAULT now(),
  c_started_at      timestamp            NULL
);

-- A second request while one is already waiting is a no-op; a request while a
-- job is 'rendering' is allowed, so edits made during a render are not lost.
CREATE UNIQUE INDEX unique_waiting_summary_job_index
  ON t_summary_job (c_program_id, c_partner)
  NULLS NOT DISTINCT
  WHERE c_state = 'pending';

-- Wake the daemon on new work.  Every other transition is its own doing.
CREATE OR REPLACE FUNCTION ch_summary_job()
  RETURNS trigger AS $$
BEGIN
  PERFORM pg_notify('ch_summary_job', NEW.c_summary_job_id::text);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_summary_job_trigger
  AFTER INSERT ON t_summary_job
  FOR EACH ROW
  EXECUTE FUNCTION ch_summary_job();
