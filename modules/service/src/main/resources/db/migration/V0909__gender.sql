CREATE TYPE e_gender AS ENUM (
  'male',
  'female',
  'other',
  'not_specified'
);

ALTER TABLE t_program_user
  ADD COLUMN c_gender e_gender;

