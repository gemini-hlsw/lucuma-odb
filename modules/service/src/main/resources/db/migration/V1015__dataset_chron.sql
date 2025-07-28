
-- Changes to datasets, for Chronnicle.
CREATE TABLE t_chron_dataset_update (

  c_chron_id       d_chron_id   NOT NULL PRIMARY KEY DEFAULT nextval('s_chron_id'),
  c_timestamp      timestamptz  NOT NULL DEFAULT current_timestamp,
  c_user           d_user_id    NULL REFERENCES t_user(c_user_id) DEFAULT current_setting('lucuma.user', true),
  c_transaction_id xid8         NOT NULL DEFAULT pg_current_xact_id(),

  c_operation      e_tg_op      NOT NULL,
  c_dataset_id     d_dataset_id NOT NULL,

  CONSTRAINT t_chron_dataset_update_unique UNIQUE (c_transaction_id, c_operation, c_dataset_id),

  c_mod_dataset_id            bool NOT NULL,
  c_mod_step_id               bool NOT NULL,
  c_mod_file_site             bool NOT NULL,
  c_mod_file_date             bool NOT NULL,
  c_mod_file_index            bool NOT NULL,
  c_mod_qa_state              bool NOT NULL,
  c_mod_start_time            bool NOT NULL,
  c_mod_end_time              bool NOT NULL,
  c_mod_observation_id        bool NOT NULL,
  c_mod_visit_id              bool NOT NULL,
  c_mod_observation_reference bool NOT NULL,
  c_mod_step_index            bool NOT NULL,
  c_mod_exposure_index        bool NOT NULL,
  c_mod_comment               bool NOT NULL,

  c_new_dataset_id            d_dataset_id,
  c_new_step_id               d_step_id,
  c_new_file_site             e_site,
  c_new_file_date             date,
  c_new_file_index            integer,
  c_new_qa_state              e_dataset_qa_state,
  c_new_start_time            timestamp without time zone,
  c_new_end_time              timestamp without time zone,
  c_new_observation_id        d_observation_id,
  c_new_visit_id              d_visit_id,
  c_new_observation_reference text,
  c_new_step_index            integer,
  c_new_exposure_index        integer,
  c_new_comment               text
);

CREATE OR REPLACE FUNCTION chron_dataset_update()
  RETURNS TRIGGER AS $$

DECLARE
  mod_dataset_id            bool := NEW.c_dataset_id            IS DISTINCT FROM OLD.c_dataset_id;
  mod_step_id               bool := NEW.c_step_id               IS DISTINCT FROM OLD.c_step_id;
  mod_file_site             bool := NEW.c_file_site             IS DISTINCT FROM OLD.c_file_site;
  mod_file_date             bool := NEW.c_file_date             IS DISTINCT FROM OLD.c_file_date;
  mod_file_index            bool := NEW.c_file_index            IS DISTINCT FROM OLD.c_file_index;
  mod_qa_state              bool := NEW.c_qa_state              IS DISTINCT FROM OLD.c_qa_state;
  mod_start_time            bool := NEW.c_start_time            IS DISTINCT FROM OLD.c_start_time;
  mod_end_time              bool := NEW.c_end_time              IS DISTINCT FROM OLD.c_end_time;
  mod_observation_id        bool := NEW.c_observation_id        IS DISTINCT FROM OLD.c_observation_id;
  mod_visit_id              bool := NEW.c_visit_id              IS DISTINCT FROM OLD.c_visit_id;
  mod_observation_reference bool := NEW.c_observation_reference IS DISTINCT FROM OLD.c_observation_reference;
  mod_step_index            bool := NEW.c_step_index            IS DISTINCT FROM OLD.c_step_index;
  mod_exposure_index        bool := NEW.c_exposure_index        IS DISTINCT FROM OLD.c_exposure_index;
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
    c_mod_qa_state,
    c_mod_start_time,
    c_mod_end_time,
    c_mod_observation_id,
    c_mod_visit_id,
    c_mod_observation_reference,
    c_mod_step_index,
    c_mod_exposure_index,
    c_mod_comment,
    c_new_dataset_id,
    c_new_step_id,
    c_new_file_site,
    c_new_file_date,
    c_new_file_index,
    c_new_qa_state,
    c_new_start_time,
    c_new_end_time,
    c_new_observation_id,
    c_new_visit_id,
    c_new_observation_reference,
    c_new_step_index,
    c_new_exposure_index,
    c_new_comment
  ) VALUES (
    TG_OP::e_tg_op,
    coalesce(OLD.c_dataset_id, NEW.c_dataset_id),
    mod_dataset_id,
    mod_step_id,
    mod_file_site,
    mod_file_date,
    mod_file_index,
    mod_qa_state,
    mod_start_time,
    mod_end_time,
    mod_observation_id,
    mod_visit_id,
    mod_observation_reference,
    mod_step_index,
    mod_exposure_index,
    mod_comment,
    CASE WHEN mod_dataset_id            THEN NEW.c_dataset_id            END,
    CASE WHEN mod_step_id               THEN NEW.c_step_id               END,
    CASE WHEN mod_file_site             THEN NEW.c_file_site             END,
    CASE WHEN mod_file_date             THEN NEW.c_file_date             END,
    CASE WHEN mod_file_index            THEN NEW.c_file_index            END,
    CASE WHEN mod_qa_state              THEN NEW.c_qa_state              END,
    CASE WHEN mod_start_time            THEN NEW.c_start_time            END,
    CASE WHEN mod_end_time              THEN NEW.c_end_time              END,
    CASE WHEN mod_observation_id        THEN NEW.c_observation_id        END,
    CASE WHEN mod_visit_id              THEN NEW.c_visit_id              END,
    CASE WHEN mod_observation_reference THEN NEW.c_observation_reference END,
    CASE WHEN mod_step_index            THEN NEW.c_step_index            END,
    CASE WHEN mod_exposure_index        THEN NEW.c_exposure_index        END,
    CASE WHEN mod_comment               THEN NEW.c_comment               END
  ) ON CONFLICT ON CONSTRAINT t_chron_dataset_update_unique DO UPDATE SET
    c_mod_dataset_id            = chron.c_mod_dataset_id            OR mod_dataset_id,
    c_mod_step_id               = chron.c_mod_step_id               OR mod_step_id,
    c_mod_file_site             = chron.c_mod_file_site             OR mod_file_site,
    c_mod_file_date             = chron.c_mod_file_date             OR mod_file_date,
    c_mod_file_index            = chron.c_mod_file_index            OR mod_file_index,
    c_mod_qa_state              = chron.c_mod_qa_state              OR mod_qa_state,
    c_mod_start_time            = chron.c_mod_start_time            OR mod_start_time,
    c_mod_end_time              = chron.c_mod_end_time              OR mod_end_time,
    c_mod_observation_id        = chron.c_mod_observation_id        OR mod_observation_id,
    c_mod_visit_id              = chron.c_mod_visit_id              OR mod_visit_id,
    c_mod_observation_reference = chron.c_mod_observation_reference OR mod_observation_reference,
    c_mod_step_index            = chron.c_mod_step_index            OR mod_step_index,
    c_mod_exposure_index        = chron.c_mod_exposure_index        OR mod_exposure_index,
    c_mod_comment               = chron.c_mod_comment               OR mod_comment,
    c_new_dataset_id            = CASE WHEN chron.c_mod_dataset_id            OR mod_dataset_id            THEN NEW.c_dataset_id            END,
    c_new_step_id               = CASE WHEN chron.c_mod_step_id               OR mod_step_id               THEN NEW.c_step_id               END,
    c_new_file_site             = CASE WHEN chron.c_mod_file_site             OR mod_file_site             THEN NEW.c_file_site             END,
    c_new_file_date             = CASE WHEN chron.c_mod_file_date             OR mod_file_date             THEN NEW.c_file_date             END,
    c_new_file_index            = CASE WHEN chron.c_mod_file_index            OR mod_file_index            THEN NEW.c_file_index            END,
    c_new_qa_state              = CASE WHEN chron.c_mod_qa_state              OR mod_qa_state              THEN NEW.c_qa_state              END,
    c_new_start_time            = CASE WHEN chron.c_mod_start_time            OR mod_start_time            THEN NEW.c_start_time            END,
    c_new_end_time              = CASE WHEN chron.c_mod_end_time              OR mod_end_time              THEN NEW.c_end_time              END,
    c_new_observation_id        = CASE WHEN chron.c_mod_observation_id        OR mod_observation_id        THEN NEW.c_observation_id        END,
    c_new_visit_id              = CASE WHEN chron.c_mod_visit_id              OR mod_visit_id              THEN NEW.c_visit_id              END,
    c_new_observation_reference = CASE WHEN chron.c_mod_observation_reference OR mod_observation_reference THEN NEW.c_observation_reference END,
    c_new_step_index            = CASE WHEN chron.c_mod_step_index            OR mod_step_index            THEN NEW.c_step_index            END,
    c_new_exposure_index        = CASE WHEN chron.c_mod_exposure_index        OR mod_exposure_index        THEN NEW.c_exposure_index        END,
    c_new_comment               = CASE WHEN chron.c_mod_comment               OR mod_comment               THEN NEW.c_comment               END;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER chron_dataset_insert_trigger
  AFTER INSERT ON t_dataset
  FOR EACH ROW
  EXECUTE PROCEDURE chron_dataset_update();

CREATE TRIGGER chron_dataset_update_trigger
  AFTER UPDATE ON t_dataset
  FOR EACH ROW
  WHEN (OLD IS DISTINCT FROM NEW)
  EXECUTE PROCEDURE chron_dataset_update();