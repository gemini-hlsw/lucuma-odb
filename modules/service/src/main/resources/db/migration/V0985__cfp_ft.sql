-- Table to add special properties for FT CFP

-- Investigator FT Role
CREATE TYPE e_ft_support_role AS ENUM (
  'reviewer',
  'mentor'
);

ALTER TABLE t_program_user
  ADD COLUMN c_ft_support_role e_ft_support_role NULL DEFAULT NULL;

-- FT CFP support users
-- FT Proposals have a single reviewer and a single mentor. It could be modeled with a single row
-- per call. However, it is more flexible to have a table with the users and their roles.
-- It would allow new roles to be added in the future, and it would allow to have multiple
-- users for the same role in the future.
-- CREATE TABLE t_ft_proposal_support (
--   c_program_id      d_program_id      NOT NULL REFERENCES t_program(c_program_id) ON DELETE CASCADE,
--   c_program_user_id d_program_user_id NOT NULL REFERENCES t_program_user(c_program_user_id),
--   c_role            e_user_ft_role    NOT NULL,
--   CONSTRAINT cfp_ft_support_unique UNIQUE (c_program_id, c_program_user_id, c_role)
-- );
--
-- COMMENT ON TABLE t_ft_proposal_support IS 'Fast Turnaround proposal support users.';
--
-- -- Verify the call is of type fast_turnaround
-- -- Verify the reviewer role
-- CREATE OR REPLACE FUNCTION fn_check_ft_support_fields()
-- RETURNS TRIGGER AS $$
-- BEGIN
--   -- Check 1: Ensure the call is of type fast_turnaround
--   IF NOT EXISTS (
--     SELECT 1 FROM v_proposal
--     WHERE c_program_id = NEW.c_program_id
--     AND c_science_subtype = 'fast_turnaround'
--   ) THEN
--     RAISE EXCEPTION 'Proposal must be of type fast turnaround';
--   END IF;
--
--   -- Check 2: If role is mentor, ensure user has PhD education
--   IF NEW.c_role = 'mentor' THEN
--     IF NOT EXISTS (
--       SELECT 1 FROM t_user
--       WHERE c_program_user_id = NEW.c_program_user_id
--       AND c_educational = 'phd'
--     ) THEN
--       RAISE EXCEPTION 'Users with mentor role must have PhD educational level';
--     END IF;
--   END IF;
--
--   RETURN NEW;
-- END;
-- $$ LANGUAGE plpgsql;
--
-- CREATE TRIGGER tr_check_cfp_type_is_fast_turnaround
-- BEFORE INSERT OR UPDATE ON t_ft_proposal_support
-- FOR EACH ROW EXECUTE FUNCTION fn_check_ft_support_fields();

-- Verify the call is of type fast_turnaround
-- Verify the mentor has a phd
CREATE OR REPLACE FUNCTION program_user_check_ft_support_fields()
RETURNS TRIGGER AS $$
BEGIN
  -- Check 1: Ensure the call is of type fast_turnaround
  IF NEW.c_ft_support_role <> NULL THEN
    IF NOT EXISTS (
      SELECT 1 FROM v_proposal
      WHERE c_program_id = NEW.c_program_id
      AND c_science_subtype = 'fast_turnaround'
    ) THEN
      RAISE EXCEPTION 'Proposal must be of type fast turnaround';
    END IF;
  END IF;

  -- Check 2: If role is mentor, ensure user has PhD education
  IF NEW.c_ft_support_role = 'mentor' and (OLD.c_educational_status <> 'phd' OR NEW.c_educational_status <> 'phd') THEN
    RAISE EXCEPTION 'Users with mentor role must have PhD educational level';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER program_user_check_ft_support_fields_trigger
BEFORE INSERT OR UPDATE ON t_program_user
FOR EACH ROW EXECUTE FUNCTION program_user_check_ft_support_fields();
