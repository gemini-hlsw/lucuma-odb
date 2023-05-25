
create sequence s_chron_id as int8; -- unique id for events
create domain d_chron_id as int8;

create type e_tg_op as enum('INSERT', 'UPDATE', 'DELETE', 'TRUNCATE');

-- A table of changes to programs, for the Chronicle.
create table t_chron_program_update (

  -- Every chronicle event has a unique id. Tables share this sequence, which is simple but could
  -- potentially result in two events of different types having the same id. :-\
  c_chron_id            d_chron_id  not null primary key default nextval('s_chron_id'),

  -- Every chronicle event has a timestamp, marking the transaction time. All events in the
  -- transaction are considered to have occurred simultaneously, which is why we need the sequence
  -- above to provide a total ordering.
  c_timestamp           timestamptz not null default current_timestamp,

  -- If we know the user (really we always should) we record that as well. We can pass it in using
  -- a transaction-local setting.
  c_user                d_user_id   null references t_user(c_user_id) default current_setting('lucuma.user', true),

  -- The transaction id is important for recording table-level events. It lets us accumulate all the
  -- updates that happen to a row in a single row here, even if it happens in multiple steps.
  c_transaction_id      xid8 not null default pg_current_xact_id(),

  -- The database operation.
  c_operation           e_tg_op not null,

  -- The event happens to a particular program. What we store here is either the new id on insert,
  -- or the old id on update, which lets us sensibly handle the odd case of changing a program id.
  c_program_id          d_program_id, 

  -- At most one row per program, per transaction, per operation (normally you only get one row
  -- per transaction, but if you were to insert and then update you would end up with two rows).
  CONSTRAINT t_chron_program_update_unique UNIQUE (c_transaction_id, c_operation, c_program_id),

  -- A set of bits telling us which fields changed. This lets us distinguish null from no action.
  c_mod_program_id      bool not null,
  c_mod_existence       bool not null,
  c_mod_pi_user_id      bool not null,
  c_mod_pi_user_type    bool not null,
  c_mod_name            bool not null,
  c_mod_pts_pi          bool not null,
  c_mod_pts_uncharged   bool not null,
  c_mod_pts_execution   bool not null,

  -- The updated properties, null if the corresponding `mod` bit is false.
  c_new_program_id      d_program_id,
  c_new_existence       e_existence,
  c_new_pi_user_id      d_user_id,
  c_new_pi_user_type    e_user_type,
  c_new_name            text,
  c_new_pts_pi          interval,
  c_new_pts_uncharged   interval,
  c_new_pts_execution   interval

);



-- The function that actually responds to the trigger and records events in the table above.
-- It's kind of long but is completely mechanical.
CREATE OR REPLACE FUNCTION chron_program_update()
  RETURNS trigger AS $$
DECLARE
  mod_program_id    bool := NEW.c_program_id    IS DISTINCT FROM OLD.c_program_id;
  mod_existence     bool := NEW.c_existence     IS DISTINCT FROM OLD.c_existence;
  mod_pi_user_id    bool := NEW.c_pi_user_id    IS DISTINCT FROM OLD.c_pi_user_id;
  mod_pi_user_type  bool := NEW.c_pi_user_type  IS DISTINCT FROM OLD.c_pi_user_type;
  mod_name          bool := NEW.c_name          IS DISTINCT FROM OLD.c_name;
  mod_pts_pi        bool := NEW.c_pts_pi        IS DISTINCT FROM OLD.c_pts_pi;
  mod_pts_uncharged bool := NEW.c_pts_uncharged IS DISTINCT FROM OLD.c_pts_uncharged;
  mod_pts_execution bool := NEW.c_pts_execution IS DISTINCT FROM OLD.c_pts_execution;
BEGIN

  INSERT INTO t_chron_program_update AS chron (
    c_operation,
    c_program_id          ,
    c_mod_program_id      ,
    c_mod_existence       ,
    c_mod_pi_user_id      ,
    c_mod_pi_user_type    ,
    c_mod_name            ,
    c_mod_pts_pi          ,
    c_mod_pts_uncharged   ,
    c_mod_pts_execution   ,
    c_new_program_id      ,
    c_new_existence       ,
    c_new_pi_user_id      ,
    c_new_pi_user_type    ,
    c_new_name            ,
    c_new_pts_pi          ,
    c_new_pts_uncharged   ,
    c_new_pts_execution
  ) VALUES (
    TG_OP::e_tg_op,
    coalesce(OLD.c_program_id, NEW.c_program_id),
    mod_program_id,   
    mod_existence,    
    mod_pi_user_id,   
    mod_pi_user_type, 
    mod_name,         
    mod_pts_pi,       
    mod_pts_uncharged,
    mod_pts_execution,
    CASE WHEN mod_program_id    = true THEN NEW.c_program_id    END,
    CASE WHEN mod_existence     = true THEN NEW.c_existence     END,
    CASE WHEN mod_pi_user_id    = true THEN NEW.c_pi_user_id    END,
    CASE WHEN mod_pi_user_type  = true THEN NEW.c_pi_user_type  END,
    CASE WHEN mod_name          = true THEN NEW.c_name          END,
    CASE WHEN mod_pts_pi        = true THEN NEW.c_pts_pi        END,
    CASE WHEN mod_pts_uncharged = true THEN NEW.c_pts_uncharged END,
    CASE WHEN mod_pts_execution = true THEN NEW.c_pts_execution END
  ) ON CONFLICT ON CONSTRAINT t_chron_program_update_unique DO UPDATE SET
    c_mod_existence     = chron.c_mod_existence     OR mod_existence,    
    c_mod_pi_user_id    = chron.c_mod_pi_user_id    OR mod_pi_user_id,   
    c_mod_pi_user_type  = chron.c_mod_pi_user_type  OR mod_pi_user_type, 
    c_mod_name          = chron.c_mod_name          OR mod_name,         
    c_mod_pts_pi        = chron.c_mod_pts_pi        OR mod_pts_pi,       
    c_mod_pts_uncharged = chron.c_mod_pts_uncharged OR mod_pts_uncharged,
    c_mod_pts_execution = chron.c_mod_pts_execution OR mod_pts_execution,
    c_new_program_id    = CASE WHEN chron.c_mod_program_id    OR mod_program_id    THEN NEW.c_program_id    END,
    c_new_existence     = CASE WHEN chron.c_mod_existence     OR mod_existence     THEN NEW.c_existence     END,
    c_new_pi_user_id    = CASE WHEN chron.c_mod_pi_user_id    OR mod_pi_user_id    THEN NEW.c_pi_user_id    END,
    c_new_pi_user_type  = CASE WHEN chron.c_mod_pi_user_type  OR mod_pi_user_type  THEN NEW.c_pi_user_type  END,
    c_new_name          = CASE WHEN chron.c_mod_name          OR mod_name          THEN NEW.c_name          END,
    c_new_pts_pi        = CASE WHEN chron.c_mod_pts_pi        OR mod_pts_pi        THEN NEW.c_pts_pi        END,
    c_new_pts_uncharged = CASE WHEN chron.c_mod_pts_uncharged OR mod_pts_uncharged THEN NEW.c_pts_uncharged END,
    c_new_pts_execution = CASE WHEN chron.c_mod_pts_execution OR mod_pts_execution THEN NEW.c_pts_execution END;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER chron_program_insert_trigger
  AFTER INSERT ON t_program
  FOR EACH ROW
  EXECUTE PROCEDURE chron_program_update();

CREATE TRIGGER chron_program_update_trigger
  AFTER UPDATE ON t_program
  FOR EACH ROW
  WHEN (OLD IS DISTINCT FROM NEW)
  EXECUTE PROCEDURE chron_program_update();
