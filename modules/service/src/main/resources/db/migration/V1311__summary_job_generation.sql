-- Expose the lifetime of a proposal summary regeneration on the program.
--
-- No new state: t_summary_job (V1309) already holds it.  This projects those
-- rows into a view with one row per program, and makes job changes fire
-- programEdit so the state rides the subscription clients already use for
-- attachments.

CREATE TYPE e_summary_generation_state AS ENUM(
  'idle',
  'pending',
  'failed'
);

CREATE INDEX summary_job_program_index ON t_summary_job (c_program_id);

-- Same shape as ch_attachment_edit (V0934), COALESCE so DELETE works.  NOTIFY
-- is delivered at commit, and finalize writes the attachment and deletes the
-- job in one transaction, so a client never sees 'idle' before the SUMMARY
-- attachments that made it idle.
CREATE OR REPLACE FUNCTION ch_program_edit_summary_job()
  RETURNS trigger AS $$
DECLARE
  job record;
BEGIN
  job := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_program_edit', job.c_program_id || ',' || 'UPDATE');
  RETURN job;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_summary_job_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_summary_job
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_program_edit_summary_job();

-- Its own view rather than more columns on v_program: v_program selects
-- `p.*`, so appending to it now fails outright, since t_program has gained
-- columns since that view was last replaced.
--
-- One row per program, so the GraphQL field can be non-null.  'pending'
-- outranks 'failed': a fresh request supersedes a stale failure.  The lateral
-- aggregate always yields a row, so a program with no jobs falls through to
-- 'idle' with no null handling.
CREATE VIEW v_summary_generation AS
  SELECT
    p.c_program_id,
    CASE WHEN sg.c_active > 0 THEN 'pending'
         WHEN sg.c_failed > 0 THEN 'failed'
         ELSE                      'idle'
    END::e_summary_generation_state                  AS c_state,
    sg.c_requested_at                                AS c_requested_at
  FROM t_program p
  LEFT JOIN LATERAL (
    SELECT count(*) FILTER (WHERE c_state IN ('pending', 'rendering')) AS c_active,
           count(*) FILTER (WHERE c_state = 'failed')                 AS c_failed,
           min(c_created_at)                                          AS c_requested_at
    FROM t_summary_job j
    WHERE j.c_program_id = p.c_program_id
  ) sg ON true;

-- The failed renders of the last regeneration, one row per partner, so the
-- client can mark the stale PDF instead of distrusting the whole set.  Empty
-- while anything is still in flight, matching v_summary_generation's 'failed'.
CREATE VIEW v_summary_failure AS
  SELECT
    j.c_summary_job_id,
    j.c_program_id,
    j.c_partner,
    COALESCE(j.c_error, 'Unknown error') AS c_error
  FROM t_summary_job j
  WHERE j.c_state = 'failed'
    AND NOT EXISTS (
      SELECT 1
      FROM t_summary_job a
      WHERE a.c_program_id = j.c_program_id
        AND a.c_state IN ('pending', 'rendering')
    );
