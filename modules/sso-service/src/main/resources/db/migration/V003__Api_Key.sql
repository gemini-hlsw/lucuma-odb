CREATE DOMAIN lucuma_api_key_id AS VARCHAR;

CREATE SEQUENCE lucuma_api_key_sequence START WITH 256;
CREATE TABLE lucuma_api_key (

  api_key_id     lucuma_api_key_id NOT NULL PRIMARY KEY,
  user_id        lucuma_user_id    NOT NULL,
  role_id        lucuma_role_id    NOT NULL,
  api_key_hash   CHAR(32)          NOT NULL,

  -- user+role must exist in lucuma_role
  FOREIGN KEY (user_id, role_id)
  REFERENCES lucuma_role (user_id, role_id)
  ON UPDATE CASCADE
  ON DELETE CASCADE

);

CREATE OR REPLACE FUNCTION insert_api_key(user_id lucuma_user_id, role_id lucuma_role_id) RETURNS text AS $$
  DECLARE
    api_key_id lucuma_api_key_id := to_hex(nextval('lucuma_api_key_sequence'::regclass));
    api_key text := md5(random()::text) || md5(random()::text) ||  md5(random()::text);
    api_key_hash text := md5(api_key);
  BEGIN
    INSERT INTO lucuma_api_key (api_key_id, user_id, role_id, api_key_hash)
    VALUES (api_key_id, user_id, role_id, api_key_hash);
    RETURN api_key_id || '.' || api_key;
  END;
$$ LANGUAGE plpgsql;
