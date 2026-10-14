-- The address replies should go to, when it differs from the sender.  Emails
-- sent on a user's behalf come from a system address (so that they pass the
-- recipient's DMARC checks) with the user as the reply-to.
ALTER TABLE t_email
  ADD COLUMN c_reply_to_email d_email NULL;
