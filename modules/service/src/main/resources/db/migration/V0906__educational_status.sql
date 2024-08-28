CREATE TYPE e_educational_status AS ENUM (
  'phd',
  'grad_student',
  'undergrad_student',
  'other'
);

ALTER TABLE t_program_user
  ADD COLUMN c_educational_status e_educational_status;

ALTER TABLE t_program_user
  ADD COLUMN c_thesis boolean;

