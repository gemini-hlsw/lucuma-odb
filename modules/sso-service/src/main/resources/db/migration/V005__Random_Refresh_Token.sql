
-- uuid_generate_v1() is a timestamp plus fixed node/clock values, so tokens were guessable.
ALTER TABLE lucuma_session
  ALTER COLUMN refresh_token SET DEFAULT gen_random_uuid();

-- Existing standard sessions still carry guessable tokens, so drop them. Guests are kept
-- because a guest cannot log back in to the same account.
DELETE FROM lucuma_session WHERE user_type = 'standard';
