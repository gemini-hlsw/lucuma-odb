-- Add fallback user profile information.
ALTER TABLE t_user
  ADD COLUMN c_fallback_given_name  VARCHAR,
  ADD COLUMN c_fallback_credit_name VARCHAR,
  ADD COLUMN c_fallback_family_name VARCHAR,
  ADD COLUMN c_fallback_email       VARCHAR;