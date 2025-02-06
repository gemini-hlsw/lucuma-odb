-- Add the new external role.
ALTER TYPE e_program_user_role ADD VALUE 'external' BEFORE 'pi';

-- Add the data access flag, defaulting to TRUE.
ALTER TABLE t_program_user
  ADD COLUMN c_has_data_access boolean NOT NULL DEFAULT TRUE;