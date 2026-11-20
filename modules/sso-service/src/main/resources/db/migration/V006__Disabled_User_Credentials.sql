-- Changing user_enabled revokes the user's sessions and API keys. This runs on re-enabling too,
-- because a JWT that outlives the disabling can still create an API key in the meantime.
CREATE FUNCTION lucuma_user_enabled_changed()
  RETURNS trigger AS $$
BEGIN
  DELETE FROM lucuma_session WHERE user_id = NEW.user_id;
  DELETE FROM lucuma_api_key WHERE user_id = NEW.user_id;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER lucuma_user_enabled_changed_trigger
  AFTER UPDATE OF user_enabled ON lucuma_user
  FOR EACH ROW
  WHEN (OLD.user_enabled IS DISTINCT FROM NEW.user_enabled)
  EXECUTE PROCEDURE lucuma_user_enabled_changed();

-- Users disabled before this trigger existed still hold credentials.
DELETE FROM lucuma_session WHERE user_id IN (SELECT user_id FROM lucuma_user WHERE NOT user_enabled);
DELETE FROM lucuma_api_key WHERE user_id IN (SELECT user_id FROM lucuma_user WHERE NOT user_enabled);
