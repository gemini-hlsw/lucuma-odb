-- Fast Turnaround (FT) Call for Proposals (CFP) Support
-- This migration adds support for FT-specific roles (reviewer and mentor) to program users

-- Create enum type for FT support roles
CREATE TYPE e_ft_support_role AS ENUM (
  'reviewer',
  'mentor'
);

-- Add FT support role column to program_user table
-- This allows users to be designated as reviewers or mentors for FT proposals
ALTER TABLE t_program_user
  ADD COLUMN c_ft_support_role e_ft_support_role NULL DEFAULT NULL;

-- Add reviewer reference to proposal table
-- Each FT proposal can have a designated reviewer
ALTER TABLE t_proposal
  ADD COLUMN c_reviewer_id d_program_user_id NULL DEFAULT NULL REFERENCES t_program_user(c_program_user_id) ON DELETE SET NULL;

-- Drop and recreate the proposal view to include FT-specific fields
DROP VIEW v_proposal;

CREATE VIEW v_proposal AS
  SELECT
    p.*,
    CASE WHEN p.c_science_subtype = 'classical'           THEN c_program_id END AS c_program_id_c,
    CASE WHEN p.c_science_subtype = 'demo_science'        THEN c_program_id END AS c_program_id_s,
    CASE WHEN p.c_science_subtype = 'directors_time'      THEN c_program_id END AS c_program_id_d,
    CASE WHEN p.c_science_subtype = 'fast_turnaround'     THEN c_program_id END AS c_program_id_f,
    CASE WHEN p.c_science_subtype = 'large_program'       THEN c_program_id END AS c_program_id_l,
    CASE WHEN p.c_science_subtype = 'poor_weather'        THEN c_program_id END AS c_program_id_p,
    CASE WHEN p.c_science_subtype = 'queue'               THEN c_program_id END AS c_program_id_q,
    CASE WHEN p.c_science_subtype = 'system_verification' THEN c_program_id END AS c_program_id_v,
    -- For FT proposals, extract the partner if there's exactly one partner split
    -- This represents the PI's affiliation for single-partner FT proposals
    CASE WHEN p.c_science_subtype = 'fast_turnaround' THEN
      (SELECT
        CASE WHEN COUNT(*) = 1 THEN MIN(ps.c_partner::d_tag) ELSE NULL::d_tag END
        FROM t_partner_split ps
       WHERE ps.c_program_id = p.c_program_id)
    END AS c_ft_partner
  FROM
    t_proposal p;
-- Trigger to validate FT support role assignments
-- Ensures: 1) FT roles are only assigned to FT proposals
--          2) Mentors must have PhD educational status
--          3) Mentor cannot be the same as reviewer on the same proposal
CREATE OR REPLACE FUNCTION program_user_check_ft_support_fields()
RETURNS TRIGGER AS $$
BEGIN
  -- Check 1: If assigning an FT support role, ensure the program has an FT proposal
  IF NEW.c_ft_support_role IS NOT NULL THEN
    IF NOT EXISTS (
      SELECT 1 FROM v_proposal
      WHERE c_program_id = NEW.c_program_id
      AND c_science_subtype = 'fast_turnaround'
    ) THEN
      RAISE EXCEPTION 'FT support roles can only be assigned to Fast Turnaround proposals';
    END IF;
  END IF;

  -- Check 2: If role is mentor, ensure user has PhD education
  IF NEW.c_ft_support_role = 'mentor' THEN
    IF NEW.c_educational_status IS DISTINCT FROM 'phd' THEN
      RAISE EXCEPTION 'Users with mentor role must have PhD educational status';
    END IF;
    
    -- Check 3: Ensure mentor is not the same as reviewer
    IF EXISTS (
      SELECT 1 FROM t_proposal p
      WHERE p.c_program_id = NEW.c_program_id
      AND p.c_reviewer_id = NEW.c_program_user_id
    ) THEN
      RAISE EXCEPTION 'A user cannot be both reviewer and mentor on the same proposal';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER program_user_check_ft_support_fields_trigger
BEFORE INSERT OR UPDATE ON t_program_user
FOR EACH ROW EXECUTE FUNCTION program_user_check_ft_support_fields();

-- Additional constraint to prevent setting a reviewer who is already a mentor
CREATE OR REPLACE FUNCTION proposal_check_reviewer_not_mentor()
RETURNS TRIGGER AS $$
BEGIN
  -- Only check for FT proposals
  IF NEW.c_reviewer_id IS NOT NULL THEN
    IF EXISTS (
      SELECT 1 FROM t_program_user pu
      WHERE pu.c_program_user_id = NEW.c_reviewer_id
      AND pu.c_program_id = NEW.c_program_id
      AND pu.c_ft_support_role = 'mentor'
    ) THEN
      RAISE EXCEPTION 'A user cannot be both reviewer and mentor on the same proposal';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER proposal_check_reviewer_not_mentor_trigger
BEFORE INSERT OR UPDATE OF c_reviewer_id ON t_proposal
FOR EACH ROW EXECUTE FUNCTION proposal_check_reviewer_not_mentor();
