
-- A table of changes to asterisms, for the Chronicle. The semantics here is "is there a row, or not?"
-- so all we support is insert and delete (not update).
create table t_chron_asterism_target_update (

  -- Common to all Chronicle tables.
  c_chron_id            d_chron_id  not null primary key default nextval('s_chron_id'),
  c_timestamp           timestamptz not null default current_timestamp,
  c_user                d_user_id   null references t_user(c_user_id) default current_setting('lucuma.user', true),
  c_transaction_id      xid8        not null default pg_current_xact_id(),
  c_operation           e_tg_op not null,

  -- Observation <-> target join.
  c_program_id          d_program_id     not null,
  c_observation_id      d_observation_id not null,
  c_target_id           d_target_id      not null
    
);

CREATE OR REPLACE FUNCTION chron_asterism_target_insert_delete()
  RETURNS trigger AS $$
BEGIN
  INSERT INTO t_chron_asterism_target_update AS chron (
    c_operation,
    c_program_id,
    c_observation_id,
    c_target_id
  ) VALUES (
    TG_OP::e_tg_op,
    coalesce(OLD.c_program_id, NEW.c_program_id),
    coalesce(OLD.c_observation_id, NEW.c_observation_id),
    coalesce(OLD.c_target_id, NEW.c_target_id)
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- turn an update into a delete + insert
CREATE OR REPLACE FUNCTION chron_asterism_target_update()
  RETURNS trigger AS $$
BEGIN
  INSERT INTO t_chron_asterism_target_update AS chron (
    c_operation,
    c_program_id,
    c_observation_id,
    c_target_id
  ) VALUES (
    'DELETE',
    OLD.c_program_id,
    OLD.c_observation_id,
    OLD.c_target_id
  );
  INSERT INTO t_chron_asterism_target_update AS chron (
    c_operation,
    c_program_id,
    c_observation_id,
    c_target_id
  ) VALUES (
    'INSERT',
    NEW.c_program_id,
    NEW.c_observation_id,
    NEW.c_target_id
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER chron_asterism_target_insert_delete_trigger
  AFTER INSERT OR DELETE ON t_asterism_target
  FOR EACH ROW
  EXECUTE PROCEDURE chron_asterism_target_insert_delete();

CREATE TRIGGER chron_asterism_target_update_trigger
  AFTER UPDATE ON t_asterism_target
  FOR EACH ROW
  EXECUTE PROCEDURE chron_asterism_target_update();
