-- The address replies should go to, when it differs from the sender.  Emails
-- sent on a user's behalf come from a system address (so that they pass the
-- recipient's DMARC checks) with the user as the reply-to.
ALTER TABLE t_email
  ADD COLUMN c_reply_to_email d_email NULL;

-- Emails along with the user id of the program's PI, which determines who may
-- see them.
CREATE VIEW v_email AS
  SELECT
    e.*,
    pi.c_user_id AS c_pi_user_id
  FROM t_email e
  LEFT JOIN t_program_user pi
    ON pi.c_program_id = e.c_program_id AND pi.c_role = 'pi';
