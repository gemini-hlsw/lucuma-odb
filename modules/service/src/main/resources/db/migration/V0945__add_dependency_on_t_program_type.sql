-- Adds a FK dependency on t_program_type.  This should cause Postgres to dump
-- and subsequently import the t_program_type table before the t_program table
-- and fix an ordering with computing program references.  

ALTER TABLE t_program
  ADD CONSTRAINT t_program_c_program_type_fkey
  FOREIGN KEY (c_program_type)
  REFERENCES t_program_type (c_type);