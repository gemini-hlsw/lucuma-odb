-- Domain for client id.  The 'c' prefix is also used for Call For Proposals,
-- but those are not UUID based.
CREATE DOMAIN d_client_id AS varchar
  CHECK (VALUE ~ '^c-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$');
COMMENT ON DOMAIN d_client_id IS 'UUID for client operations like event recording';

-- Adds an optional client id column, which if specified must be unique.
ALTER TABLE t_execution_event
  ADD COLUMN c_client_id d_client_id,
  ADD CONSTRAINT unique_client_id UNIQUE (c_client_id);