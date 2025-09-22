-- Adds an idempotency key to take the place of client id.
ALTER TABLE t_execution_event
  ADD COLUMN c_idempotency_key uuid;

-- Probably not necessary, but we'll move the existing client id to idempotency
-- key.
UPDATE t_execution_event
   SET c_idempotency_key = substring(c_client_id from 3)::uuid;

-- Make it unique.
ALTER TABLE t_execution_event
  ADD CONSTRAINT unique_idempotency_key unique (c_idempotency_key);

-- Drop the client id.
ALTER TABLE t_execution_event
  DROP COLUMN c_client_id;
