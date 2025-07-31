ALTER TABLE t_program_user
  ADD COLUMN c_affiliation VARCHAR,
  ADD CONSTRAINT c_affiliation_not_empty CHECK (c_affiliation IS NULL OR length(trim(c_affiliation)) > 0);
