-- Freeze ITC results at execution start, split into acquisition and science.
--
-- Once an observation begins execution its sequence is materialized to the
-- database and executed faithfully regardless of later edits.  The ITC results
-- that fed that generation are treated the same way: frozen and authoritative.
-- A frozen row is exempt from the wholesale wipe on an ITC version bump and is
-- returned by lookups regardless of the input hash, so an ITC outage or version
-- change can no longer strip an executing observation of its ITC results.
--
-- The two parts of a result are stored, cached, and frozen independently:
--
--   * Science results exist for every observing mode and, once frozen, are
--     authoritative for the life of the observation.
--
--   * Acquisition results exist only for the modes that have an acquisition
--     sequence (the long slit spectroscopy modes).  They are likewise frozen at
--     execution start, but are additionally re-derived by the resetAcquisition
--     mutation (the only thing that consults the ITC for acquisition once
--     execution has begun).
--
-- Both parts share the single c_hash (md5 of the full ITC input) for
-- pre-execution cache validity and the single c_is_frozen flag (both freeze
-- together at execution start).  t_itc_result is a disposable cache and nothing
-- released depends on its contents, so we simply wipe it and restructure the
-- result columns rather than migrate the old fused blob.
TRUNCATE TABLE t_itc_result;

ALTER TABLE t_itc_result
  DROP CONSTRAINT t_itc_result_one_result,
  DROP COLUMN c_asterism_results,
  DROP COLUMN c_error_message,

  ADD COLUMN c_is_frozen           boolean NOT NULL DEFAULT false,
  ADD COLUMN c_acquisition_results jsonb,
  ADD COLUMN c_acquisition_error   text,
  ADD COLUMN c_science_results     jsonb,
  ADD COLUMN c_science_error       text,

  -- Science always applies: exactly one of results / deterministic error.
  ADD CONSTRAINT t_itc_result_science
    CHECK (num_nonnulls(c_science_results, c_science_error) = 1),

  -- Acquisition is optional (both null = a mode with no acquisition sequence);
  -- when present it is either a success or a cached deterministic failure.
  ADD CONSTRAINT t_itc_result_acquisition
    CHECK (num_nonnulls(c_acquisition_results, c_acquisition_error) <= 1);

-- On an ITC version change, wipe only the (unfrozen) cache rows and keep frozen
-- results belonging to executing/executed observations.  Previously this was a
-- wholesale TRUNCATE.  The obscalc reset below is unchanged (it already skips
-- ongoing/completed observations).
CREATE OR REPLACE FUNCTION itc_version_update()
  RETURNS trigger AS $$
BEGIN
  NEW.c_last_update = NOW();
  IF (OLD.c_version IS DISTINCT FROM NEW.c_version OR OLD.c_data IS DISTINCT FROM NEW.c_data) THEN
    DELETE FROM t_itc_result WHERE NOT c_is_frozen;

    -- Reset to pending, but only for observations in the 'ready' workflow state or non-executed
    -- ones with itc errors.
    UPDATE t_obscalc SET
      c_last_invalidation = NOW(),
      c_failure_count     = 0,
      c_retry_at          = NULL,
      c_obscalc_state     = 'pending'
    WHERE c_obscalc_state IN ('ready', 'retry')
      AND c_workflow_state NOT IN ('inactive', 'ongoing', 'completed')
      AND (
            c_workflow_state = 'ready'
            OR c_workflow_validations @> '[{"code": "ITC_ERROR"}]'::jsonb
          );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
