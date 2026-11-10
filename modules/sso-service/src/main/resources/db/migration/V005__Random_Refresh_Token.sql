
-- uuid_generate_v1() is a timestamp plus fixed node/clock values, so tokens were guessable.
ALTER TABLE lucuma_session
  ALTER COLUMN refresh_token SET DEFAULT gen_random_uuid();
