-- Adds the 'superseded' trigger status, in a migration of its own.
--
-- A trigger records the ToO activation it was requested at (V1267), and that
-- value never changes: an activation at a different level is a different request,
-- because who is notified, how fast, and what they are expected to drop all
-- differ.  So when the observation's activation moves while a request is
-- outstanding, the outstanding row is closed out and replaced rather than
-- amended, and 'superseded' is what it is closed out as.
--
ALTER TYPE e_too_trigger_status ADD VALUE 'superseded';

COMMENT ON TYPE e_too_trigger_status IS
  'ToO trigger lifecycle. requested is the only non-terminal status; declined '
  '(observer said no), withdrawn (PI took it back) and superseded (replaced by a '
  'request at a different activation) are terminal, and all three are equivalent '
  'to never having been triggered.';
