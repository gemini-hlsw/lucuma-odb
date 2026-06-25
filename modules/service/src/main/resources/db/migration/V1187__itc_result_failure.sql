-- Persistent cache for deterministic ITC failures.
-- Keyed by (program_id, observation_id) with a hash column matching the ITC input hash.
--
-- A hash mismatch means the observation changed, so the cached failure is ignored and
-- ITC is re-called.
CREATE TABLE t_itc_result_failure (
  c_program_id      d_program_id     NOT NULL,
  c_observation_id  d_observation_id NOT NULL,
  c_hash            bytea            NOT NULL,
  c_error_message   text             NOT NULL,
  FOREIGN KEY (c_program_id, c_observation_id)
    REFERENCES t_observation(c_program_id, c_observation_id)
    ON DELETE CASCADE,
  CONSTRAINT t_itc_result_failure_pkey PRIMARY KEY (c_program_id, c_observation_id)
);

-- Extend the ITC version trigger to also clear failures when ITC is updated.
CREATE OR REPLACE FUNCTION itc_version_update()
  RETURNS trigger AS $$
BEGIN
  NEW.c_last_update = NOW();
  IF (OLD.c_version IS DISTINCT FROM NEW.c_version OR OLD.c_data IS DISTINCT FROM NEW.c_data) THEN
    TRUNCATE t_itc_result;
    TRUNCATE t_itc_result_failure;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
