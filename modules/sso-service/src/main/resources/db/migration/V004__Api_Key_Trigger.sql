

-- Whenever an API key is deleted, notify channel 'lucuma_api_key_deleted' with the key's id

CREATE OR REPLACE FUNCTION lucuma_api_key_deleted()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('lucuma_api_key_deleted', OLD.api_key_id);
  RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER lucuma_api_key_deleted_trigger
  AFTER DELETE ON lucuma_api_key
  FOR EACH ROW
  EXECUTE PROCEDURE lucuma_api_key_deleted();

