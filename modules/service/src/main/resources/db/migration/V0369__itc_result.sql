--
-- ITC version table (one row for now).
--
CREATE TABLE t_itc_version (
  c_last_update timestamptz NOT NULL,
  c_version     text        NULL,
  c_data        text        NULL
);

--
-- The version table will have a single row.  Initialize so that it may be
-- simply updated thereafter.
--
INSERT INTO t_itc_version (
  c_last_update
) VALUES (
  NOW()
);

--
-- ITC result table associated with a hash of ITC inputs.
--
CREATE TABLE t_itc_result (
  c_program_id      d_program_id     NOT NULL,
  c_observation_id  d_observation_id NOT NULL,
  c_target_id       d_target_id      NOT NULL,
  c_hash            text             NOT NULL,
  c_exposure_time   interval         NOT NULL,
  c_exposure_count  int4             NOT NULL,
  c_signal_to_noise numeric(10,3)    NOT NULL,
  FOREIGN KEY (c_program_id, c_observation_id, c_target_id)
    REFERENCES t_asterism_target(c_program_id, c_observation_id, c_target_id)
    ON DELETE CASCADE,
  CONSTRAINT t_itc_result_pkey PRIMARY KEY (c_program_id, c_observation_id, c_target_id)
);

--
-- A function that handles truncating the ITC result table when the ITC
-- version data is updated.
--
CREATE FUNCTION itc_version_update()
  RETURNS trigger AS $$
BEGIN
  NEW.c_last_update = NOW();
  IF (OLD.c_version IS DISTINCT FROM NEW.c_version OR OLD.c_data IS DISTINCT FROM NEW.c_data) THEN
    TRUNCATE t_itc_result;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER t_itc_version_update_trigger
BEFORE UPDATE on t_itc_version
FOR EACH ROW
EXECUTE FUNCTION itc_version_update();
