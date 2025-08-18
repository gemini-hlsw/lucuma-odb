
-- We need uuids.
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Abandon the idea of a 'current role' for a user.
ALTER TABLE lucuma_user DROP CONSTRAINT lucuma_user_role_fk;
ALTER TABLE lucuma_user DROP COLUMN role_id;

-- And add the idea of sessions, which are associated with (a) guest users, and (b) standard users
-- in a specific role. A user may be logged in on many devices under the same role or in different
-- roles.
CREATE TABLE lucuma_session (

  refresh_token uuid             NOT NULL PRIMARY KEY DEFAULT uuid_generate_v1(),
  user_id       lucuma_user_id   NOT NULL,
  user_type     lucuma_user_type NOT NULL,
  role_id       lucuma_role_id,

  -- user+type must exist in lucuma_user
  CHECK (user_type = 'standard' or user_type = 'guest'),
  FOREIGN KEY (user_id, user_type)
  REFERENCES lucuma_user (user_id, user_type)
  ON UPDATE CASCADE
  ON DELETE CASCADE,

  -- if a role is specified (FK doesn't apply otherwise) it must exist in lucuma_role
  FOREIGN KEY (user_id, role_id)
  REFERENCES lucuma_role (user_id, role_id)
  ON UPDATE CASCADE
  ON DELETE CASCADE,

  -- guest xor role
  CHECK (user_type = 'guest' OR role_id IS NOT NULL)

);

-- So this means when we upgrade a guest user we must first delete the refresh tokens, upgrade the
-- user, then re-insert the refresh tokens with the new role. Otherwise we'll get a check constraint
-- violation because the cascade won't work.

