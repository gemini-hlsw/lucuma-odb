-- The Proposal Status

create table t_proposal_status (
  c_tag       d_tag      not null primary key,
  c_name      varchar    not null,
  c_ordinal   smallint   not null unique
);

insert into t_proposal_status (c_tag, c_name, c_ordinal) values ('not_submitted', 'Not Submitted', 0);
insert into t_proposal_status (c_tag, c_name, c_ordinal) values ('submitted', 'Submitted', 1);
insert into t_proposal_status (c_tag, c_name, c_ordinal) values ('accepted', 'Accepted', 2);
insert into t_proposal_status (c_tag, c_name, c_ordinal) values ('not_accepted', 'Not Accepted', 3);

alter table t_program 
add column c_proposal_status d_tag not null default 'not_submitted' references t_proposal_status(c_tag);

-- If a proposal is deleted, we want to reset the proposal status to not_accepted.
-- (proposals cannot be deleted via the API, at least not currently, but just in case...)
create or replace function reset_proposal_status()
  returns trigger as $$
begin
  update t_program
  set c_proposal_status = 'not_submitted'
  where c_program_id = OLD.c_program_id;
  return NEW;
end;
$$ language plpgsql;

create trigger reset_proposal_status_trigger
  after delete on t_proposal
  for each row execute procedure reset_proposal_status();

-- Add the proposalStatus columns to t_chron_program_update
alter table t_chron_program_update
add column c_mod_proposal_status  bool not null,
add column c_new_proposal_status d_tag;

-- This function was copied and modified from V0361__chron_program_updates.sql

-- The function that actually responds to the trigger and records events in the t_chron_program_update table.
-- It's kind of long but is completely mechanical.
CREATE OR REPLACE FUNCTION chron_program_update()
  RETURNS trigger AS $$
DECLARE
  mod_program_id      bool := NEW.c_program_id      IS DISTINCT FROM OLD.c_program_id;
  mod_existence       bool := NEW.c_existence       IS DISTINCT FROM OLD.c_existence;
  mod_pi_user_id      bool := NEW.c_pi_user_id      IS DISTINCT FROM OLD.c_pi_user_id;
  mod_pi_user_type    bool := NEW.c_pi_user_type    IS DISTINCT FROM OLD.c_pi_user_type;
  mod_name            bool := NEW.c_name            IS DISTINCT FROM OLD.c_name;
  mod_pts_pi          bool := NEW.c_pts_pi          IS DISTINCT FROM OLD.c_pts_pi;
  mod_pts_uncharged   bool := NEW.c_pts_uncharged   IS DISTINCT FROM OLD.c_pts_uncharged;
  mod_pts_execution   bool := NEW.c_pts_execution   IS DISTINCT FROM OLD.c_pts_execution;
  mod_proposal_status bool := NEW.c_proposal_status IS DISTINCT FROM OLD.c_proposal_status;
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
    c_mod_proposal_status ,
    c_new_program_id      ,
    c_new_existence       ,
    c_new_pi_user_id      ,
    c_new_pi_user_type    ,
    c_new_name            ,
    c_new_pts_pi          ,
    c_new_pts_uncharged   ,
    c_new_pts_execution   ,
    c_new_proposal_status
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
    mod_proposal_status,
    CASE WHEN mod_program_id      = true THEN NEW.c_program_id      END,
    CASE WHEN mod_existence       = true THEN NEW.c_existence       END,
    CASE WHEN mod_pi_user_id      = true THEN NEW.c_pi_user_id      END,
    CASE WHEN mod_pi_user_type    = true THEN NEW.c_pi_user_type    END,
    CASE WHEN mod_name            = true THEN NEW.c_name            END,
    CASE WHEN mod_pts_pi          = true THEN NEW.c_pts_pi          END,
    CASE WHEN mod_pts_uncharged   = true THEN NEW.c_pts_uncharged   END,
    CASE WHEN mod_pts_execution   = true THEN NEW.c_pts_execution   END,
    CASE WHEN mod_proposal_status = true THEN NEW.c_proposal_status END
  ) ON CONFLICT ON CONSTRAINT t_chron_program_update_unique DO UPDATE SET
    c_mod_existence       = chron.c_mod_existence       OR mod_existence,    
    c_mod_pi_user_id      = chron.c_mod_pi_user_id      OR mod_pi_user_id,   
    c_mod_pi_user_type    = chron.c_mod_pi_user_type    OR mod_pi_user_type, 
    c_mod_name            = chron.c_mod_name            OR mod_name,         
    c_mod_pts_pi          = chron.c_mod_pts_pi          OR mod_pts_pi,       
    c_mod_pts_uncharged   = chron.c_mod_pts_uncharged   OR mod_pts_uncharged,
    c_mod_pts_execution   = chron.c_mod_pts_execution   OR mod_pts_execution,
    c_mod_proposal_status = chron.c_mod_proposal_status OR mod_proposal_status,
    c_new_program_id      = CASE WHEN chron.c_mod_program_id      OR mod_program_id      THEN NEW.c_program_id    END,
    c_new_existence       = CASE WHEN chron.c_mod_existence       OR mod_existence       THEN NEW.c_existence     END,
    c_new_pi_user_id      = CASE WHEN chron.c_mod_pi_user_id      OR mod_pi_user_id      THEN NEW.c_pi_user_id    END,
    c_new_pi_user_type    = CASE WHEN chron.c_mod_pi_user_type    OR mod_pi_user_type    THEN NEW.c_pi_user_type  END,
    c_new_name            = CASE WHEN chron.c_mod_name            OR mod_name            THEN NEW.c_name          END,
    c_new_pts_pi          = CASE WHEN chron.c_mod_pts_pi          OR mod_pts_pi          THEN NEW.c_pts_pi        END,
    c_new_pts_uncharged   = CASE WHEN chron.c_mod_pts_uncharged   OR mod_pts_uncharged   THEN NEW.c_pts_uncharged END,
    c_new_pts_execution   = CASE WHEN chron.c_mod_pts_execution   OR mod_pts_execution   THEN NEW.c_pts_execution END,
    c_new_proposal_status = CASE WHEN chron.c_mod_proposal_status OR mod_proposal_status THEN NEW.c_proposal_status END;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
