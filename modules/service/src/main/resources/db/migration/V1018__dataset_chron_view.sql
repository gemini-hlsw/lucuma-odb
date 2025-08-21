-- Starting over with the dataset chron.
TRUNCATE TABLE t_chron_dataset_update;

-- Just add the generated columns as if they were normal columns.
ALTER TABLE t_chron_dataset_update
  ADD COLUMN c_mod_filename          bool     NOT NULL,
  ADD COLUMN c_mod_dataset_reference bool     NOT NULL,
  ADD COLUMN c_new_filename          varchar,
  ADD COLUMN c_new_dataset_reference text;

-- Update the update function so that it includes the new columns.
CREATE OR REPLACE FUNCTION chron_dataset_update()
  RETURNS TRIGGER AS $$

DECLARE
  mod_dataset_id            bool := NEW.c_dataset_id            IS DISTINCT FROM OLD.c_dataset_id;
  mod_step_id               bool := NEW.c_step_id               IS DISTINCT FROM OLD.c_step_id;
  mod_file_site             bool := NEW.c_file_site             IS DISTINCT FROM OLD.c_file_site;
  mod_file_date             bool := NEW.c_file_date             IS DISTINCT FROM OLD.c_file_date;
  mod_file_index            bool := NEW.c_file_index            IS DISTINCT FROM OLD.c_file_index;
  mod_filename              bool := NEW.c_filename              IS DISTINCT FROM OLD.c_filename;
  mod_qa_state              bool := NEW.c_qa_state              IS DISTINCT FROM OLD.c_qa_state;
  mod_start_time            bool := NEW.c_start_time            IS DISTINCT FROM OLD.c_start_time;
  mod_end_time              bool := NEW.c_end_time              IS DISTINCT FROM OLD.c_end_time;
  mod_observation_id        bool := NEW.c_observation_id        IS DISTINCT FROM OLD.c_observation_id;
  mod_visit_id              bool := NEW.c_visit_id              IS DISTINCT FROM OLD.c_visit_id;
  mod_observation_reference bool := NEW.c_observation_reference IS DISTINCT FROM OLD.c_observation_reference;
  mod_step_index            bool := NEW.c_step_index            IS DISTINCT FROM OLD.c_step_index;
  mod_exposure_index        bool := NEW.c_exposure_index        IS DISTINCT FROM OLD.c_exposure_index;
  mod_dataset_reference     bool := NEW.c_dataset_reference     IS DISTINCT FROM OLD.c_dataset_reference;
  mod_comment               bool := NEW.c_comment               IS DISTINCT FROM OLD.c_comment;

BEGIN
  INSERT INTO t_chron_dataset_update AS chron (
    c_operation,
    c_dataset_id,
    c_mod_dataset_id,
    c_mod_step_id,
    c_mod_file_site,
    c_mod_file_date,
    c_mod_file_index,
    c_mod_filename,
    c_mod_qa_state,
    c_mod_start_time,
    c_mod_end_time,
    c_mod_observation_id,
    c_mod_visit_id,
    c_mod_observation_reference,
    c_mod_step_index,
    c_mod_exposure_index,
    c_mod_dataset_reference,
    c_mod_comment,
    c_new_dataset_id,
    c_new_step_id,
    c_new_file_site,
    c_new_file_date,
    c_new_file_index,
    c_new_filename,
    c_new_qa_state,
    c_new_start_time,
    c_new_end_time,
    c_new_observation_id,
    c_new_visit_id,
    c_new_observation_reference,
    c_new_step_index,
    c_new_exposure_index,
    c_new_dataset_reference,
    c_new_comment
  ) VALUES (
    TG_OP::e_tg_op,
    coalesce(OLD.c_dataset_id, NEW.c_dataset_id),
    mod_dataset_id,
    mod_step_id,
    mod_file_site,
    mod_file_date,
    mod_file_index,
    mod_filename,
    mod_qa_state,
    mod_start_time,
    mod_end_time,
    mod_observation_id,
    mod_visit_id,
    mod_observation_reference,
    mod_step_index,
    mod_exposure_index,
    mod_dataset_reference,
    mod_comment,
    CASE WHEN mod_dataset_id            THEN NEW.c_dataset_id            END,
    CASE WHEN mod_step_id               THEN NEW.c_step_id               END,
    CASE WHEN mod_file_site             THEN NEW.c_file_site             END,
    CASE WHEN mod_file_date             THEN NEW.c_file_date             END,
    CASE WHEN mod_file_index            THEN NEW.c_file_index            END,
    CASE WHEN mod_filename              THEN NEW.c_filename              END,
    CASE WHEN mod_qa_state              THEN NEW.c_qa_state              END,
    CASE WHEN mod_start_time            THEN NEW.c_start_time            END,
    CASE WHEN mod_end_time              THEN NEW.c_end_time              END,
    CASE WHEN mod_observation_id        THEN NEW.c_observation_id        END,
    CASE WHEN mod_visit_id              THEN NEW.c_visit_id              END,
    CASE WHEN mod_observation_reference THEN NEW.c_observation_reference END,
    CASE WHEN mod_step_index            THEN NEW.c_step_index            END,
    CASE WHEN mod_exposure_index        THEN NEW.c_exposure_index        END,
    CASE WHEN mod_dataset_reference     THEN NEW.c_dataset_reference     END,
    CASE WHEN mod_comment               THEN NEW.c_comment               END
  ) ON CONFLICT ON CONSTRAINT t_chron_dataset_update_unique DO UPDATE SET
    c_mod_dataset_id            = chron.c_mod_dataset_id            OR mod_dataset_id,
    c_mod_step_id               = chron.c_mod_step_id               OR mod_step_id,
    c_mod_file_site             = chron.c_mod_file_site             OR mod_file_site,
    c_mod_file_date             = chron.c_mod_file_date             OR mod_file_date,
    c_mod_file_index            = chron.c_mod_file_index            OR mod_file_index,
    c_mod_filename              = chron.c_mod_filename              OR mod_filename,
    c_mod_qa_state              = chron.c_mod_qa_state              OR mod_qa_state,
    c_mod_start_time            = chron.c_mod_start_time            OR mod_start_time,
    c_mod_end_time              = chron.c_mod_end_time              OR mod_end_time,
    c_mod_observation_id        = chron.c_mod_observation_id        OR mod_observation_id,
    c_mod_visit_id              = chron.c_mod_visit_id              OR mod_visit_id,
    c_mod_observation_reference = chron.c_mod_observation_reference OR mod_observation_reference,
    c_mod_step_index            = chron.c_mod_step_index            OR mod_step_index,
    c_mod_exposure_index        = chron.c_mod_exposure_index        OR mod_exposure_index,
    c_mod_dataset_reference     = chron.c_mod_dataset_reference     OR mod_dataset_reference,
    c_mod_comment               = chron.c_mod_comment               OR mod_comment,
    c_new_dataset_id            = CASE WHEN chron.c_mod_dataset_id            OR mod_dataset_id            THEN NEW.c_dataset_id            END,
    c_new_step_id               = CASE WHEN chron.c_mod_step_id               OR mod_step_id               THEN NEW.c_step_id               END,
    c_new_file_site             = CASE WHEN chron.c_mod_file_site             OR mod_file_site             THEN NEW.c_file_site             END,
    c_new_file_date             = CASE WHEN chron.c_mod_file_date             OR mod_file_date             THEN NEW.c_file_date             END,
    c_new_file_index            = CASE WHEN chron.c_mod_file_index            OR mod_file_index            THEN NEW.c_file_index            END,
    c_new_filename              = CASE WHEN chron.c_mod_filename              OR mod_filename              THEN NEW.c_filename              END,
    c_new_qa_state              = CASE WHEN chron.c_mod_qa_state              OR mod_qa_state              THEN NEW.c_qa_state              END,
    c_new_start_time            = CASE WHEN chron.c_mod_start_time            OR mod_start_time            THEN NEW.c_start_time            END,
    c_new_end_time              = CASE WHEN chron.c_mod_end_time              OR mod_end_time              THEN NEW.c_end_time              END,
    c_new_observation_id        = CASE WHEN chron.c_mod_observation_id        OR mod_observation_id        THEN NEW.c_observation_id        END,
    c_new_visit_id              = CASE WHEN chron.c_mod_visit_id              OR mod_visit_id              THEN NEW.c_visit_id              END,
    c_new_observation_reference = CASE WHEN chron.c_mod_observation_reference OR mod_observation_reference THEN NEW.c_observation_reference END,
    c_new_step_index            = CASE WHEN chron.c_mod_step_index            OR mod_step_index            THEN NEW.c_step_index            END,
    c_new_exposure_index        = CASE WHEN chron.c_mod_exposure_index        OR mod_exposure_index        THEN NEW.c_exposure_index        END,
    c_new_dataset_reference     = CASE WHEN chron.c_mod_dataset_reference     OR mod_dataset_reference     THEN NEW.c_dataset_reference     END,
    c_new_comment               = CASE WHEN chron.c_mod_comment               OR mod_comment               THEN NEW.c_comment               END;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


-- Add a view that synthesizes a modification flag for the interval and adds
-- the coalesced start and end time (so that the new interval can be calculated
-- from the data in the view).
CREATE OR REPLACE VIEW v_chron_dataset_update AS
SELECT
  u.*,

  -- dataset interval from its constituent parts
  (u.c_mod_start_time OR u.c_mod_end_time)     AS c_mod_interval,

  COALESCE(u.c_new_start_time, d.c_start_time) AS c_coal_start_time,
  COALESCE(u.c_new_end_time,   d.c_end_time)   AS c_coal_end_time

FROM
  t_chron_dataset_update u
INNER JOIN
  t_dataset d ON d.c_dataset_id = u.c_dataset_id;