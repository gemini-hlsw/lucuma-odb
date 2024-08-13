-- There can only be one PI per program.
CREATE UNIQUE INDEX unique_pi_role_per_program
  ON t_program_user(c_program_id)
  WHERE c_role = 'pi';

-- Drop the standard user constraint on t_program_user.  Since PIs can be guest
-- users, we have to adjust it.
ALTER TABLE t_program_user
  DROP CONSTRAINT "t_program_user_c_user_type_check",
  ADD CONSTRAINT "t_program_user_c_user_type_check" CHECK (
    c_user_type = 'standard'::e_user_type OR
    (c_role = 'pi'::e_program_user_role AND c_user_type = 'guest'::e_user_type)
  );

-- Add the PI to the program user table.
INSERT INTO t_program_user (
  c_program_id,
  c_user_id,
  c_user_type,
  c_role,
  c_partner,
  c_partner_link
)
SELECT
  c_program_id,
  c_pi_user_id,
  c_pi_user_type,
  'pi'::e_program_user_role,
  null::d_tag,
  'has_unspecified_partner'::e_partner_link
FROM t_program p
WHERE p.c_pi_user_id IS NOT NULL;


ALTER TABLE t_chron_program_update
  DROP COLUMN c_mod_pi_user_id,
  DROP COLUMN c_mod_pi_user_type,
  DROP COLUMN c_new_pi_user_id,
  DROP COLUMN c_new_pi_user_type;

CREATE OR REPLACE FUNCTION chron_program_update()
  RETURNS trigger AS $$
DECLARE
  mod_program_id      bool := NEW.c_program_id      IS DISTINCT FROM OLD.c_program_id;
  mod_existence       bool := NEW.c_existence       IS DISTINCT FROM OLD.c_existence;
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
    c_mod_name            ,
    c_mod_pts_pi          ,
    c_mod_pts_uncharged   ,
    c_mod_pts_execution   ,
    c_mod_proposal_status ,
    c_new_program_id      ,
    c_new_existence       ,
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
    mod_name,
    mod_pts_pi,
    mod_pts_uncharged,
    mod_pts_execution,
    mod_proposal_status,
    CASE WHEN mod_program_id      = true THEN NEW.c_program_id      END,
    CASE WHEN mod_existence       = true THEN NEW.c_existence       END,
    CASE WHEN mod_name            = true THEN NEW.c_name            END,
    CASE WHEN mod_pts_pi          = true THEN NEW.c_pts_pi          END,
    CASE WHEN mod_pts_uncharged   = true THEN NEW.c_pts_uncharged   END,
    CASE WHEN mod_pts_execution   = true THEN NEW.c_pts_execution   END,
    CASE WHEN mod_proposal_status = true THEN NEW.c_proposal_status END
  ) ON CONFLICT ON CONSTRAINT t_chron_program_update_unique DO UPDATE SET
    c_mod_existence       = chron.c_mod_existence       OR mod_existence,
    c_mod_name            = chron.c_mod_name            OR mod_name,
    c_mod_pts_pi          = chron.c_mod_pts_pi          OR mod_pts_pi,
    c_mod_pts_uncharged   = chron.c_mod_pts_uncharged   OR mod_pts_uncharged,
    c_mod_pts_execution   = chron.c_mod_pts_execution   OR mod_pts_execution,
    c_mod_proposal_status = chron.c_mod_proposal_status OR mod_proposal_status,
    c_new_program_id      = CASE WHEN chron.c_mod_program_id      OR mod_program_id      THEN NEW.c_program_id    END,
    c_new_existence       = CASE WHEN chron.c_mod_existence       OR mod_existence       THEN NEW.c_existence     END,
    c_new_name            = CASE WHEN chron.c_mod_name            OR mod_name            THEN NEW.c_name          END,
    c_new_pts_pi          = CASE WHEN chron.c_mod_pts_pi          OR mod_pts_pi          THEN NEW.c_pts_pi        END,
    c_new_pts_uncharged   = CASE WHEN chron.c_mod_pts_uncharged   OR mod_pts_uncharged   THEN NEW.c_pts_uncharged END,
    c_new_pts_execution   = CASE WHEN chron.c_mod_pts_execution   OR mod_pts_execution   THEN NEW.c_pts_execution END,
    c_new_proposal_status = CASE WHEN chron.c_mod_proposal_status OR mod_proposal_status THEN NEW.c_proposal_status END;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Drop the PI from the program.
ALTER TABLE t_program
  DROP CONSTRAINT "t_program_c_pi_user_type_check",
  DROP COLUMN c_pi_user_id,
  DROP COLUMN c_pi_user_type;