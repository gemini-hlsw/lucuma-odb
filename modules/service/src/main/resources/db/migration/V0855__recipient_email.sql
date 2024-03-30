
-- Update recipient email regex check

ALTER DOMAIN d_email DROP CONSTRAINT d_email_check;
ALTER DOMAIN d_email ADD CONSTRAINT d_email_check CHECK(
  --  N.B. this is the same pattern used in the EmailAddress class; if we change one we need to change the other.
  VALUE ~ '^[a-zA-Z0-9_+&-]+(?:.[a-zA-Z0-9_+&-]+)*@(?:[a-zA-Z0-9-]+.)+[a-zA-Z]{2,7}$'
);
