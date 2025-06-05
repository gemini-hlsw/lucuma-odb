-- Fast Turnaround (FT) Call for Proposals (CFP) Support
-- This migration adds unified support for FT reviewer and mentor assignment via proposal fields

-- Add reviewer and mentor references to proposal table
-- Each FT proposal can have designated reviewer and mentor
ALTER TABLE t_proposal
  ADD COLUMN c_reviewer_id d_program_user_id NULL DEFAULT NULL REFERENCES t_program_user(c_program_user_id) ON DELETE SET NULL,
  ADD COLUMN c_mentor_id d_program_user_id NULL DEFAULT NULL REFERENCES t_program_user(c_program_user_id) ON DELETE SET NULL;

-- Add indexes for performance
CREATE INDEX ix_proposal_reviewer_id ON t_proposal(c_reviewer_id);
CREATE INDEX ix_proposal_mentor_id ON t_proposal(c_mentor_id);

-- Add constraint to prevent same user being both reviewer and mentor
ALTER TABLE t_proposal 
ADD CONSTRAINT chk_reviewer_mentor_different 
CHECK (c_reviewer_id IS NULL OR c_mentor_id IS NULL OR c_reviewer_id != c_mentor_id);

-- Add comments
COMMENT ON COLUMN t_proposal.c_reviewer_id IS 'Program user assigned as reviewer for FT proposals';
COMMENT ON COLUMN t_proposal.c_mentor_id IS 'Program user assigned as mentor for FT proposals';
COMMENT ON CONSTRAINT chk_reviewer_mentor_different ON t_proposal IS 'Prevent same user from being both reviewer and mentor on a proposal';

-- Drop and recreate the proposal view to include FT-specific fields
DROP VIEW v_proposal;

CREATE VIEW v_proposal AS
  SELECT
    p.*,
    m.c_name AS c_title,
    m.c_description AS c_abstract,
    CASE WHEN p.c_science_subtype = 'classical'           THEN p.c_program_id END AS c_program_id_c,
    CASE WHEN p.c_science_subtype = 'demo_science'        THEN p.c_program_id END AS c_program_id_s,
    CASE WHEN p.c_science_subtype = 'directors_time'      THEN p.c_program_id END AS c_program_id_d,
    CASE WHEN p.c_science_subtype = 'fast_turnaround'     THEN p.c_program_id END AS c_program_id_f,
    CASE WHEN p.c_science_subtype = 'large_program'       THEN p.c_program_id END AS c_program_id_l,
    CASE WHEN p.c_science_subtype = 'poor_weather'        THEN p.c_program_id END AS c_program_id_p,
    CASE WHEN p.c_science_subtype = 'queue'               THEN p.c_program_id END AS c_program_id_q,
    CASE WHEN p.c_science_subtype = 'system_verification' THEN p.c_program_id END AS c_program_id_v,
    CASE WHEN p.c_science_subtype = 'fast_turnaround' THEN
      (SELECT
        CASE WHEN COUNT(*) = 1 THEN MIN(ps.c_partner::d_tag) ELSE NULL::d_tag END
        FROM t_partner_split ps
       WHERE ps.c_program_id = p.c_program_id)
    END AS c_ft_partner
  FROM
    t_proposal p
    INNER JOIN t_program m ON p.c_program_id = m.c_program_id;

-- Trigger to clear reviewer and mentor when program user is removed
CREATE OR REPLACE FUNCTION cleanup_ft_roles_on_user_removal()
RETURNS TRIGGER AS $$
BEGIN
  -- Clear reviewer_id in proposals where this user was the reviewer
  UPDATE t_proposal 
  SET c_reviewer_id = NULL 
  WHERE c_reviewer_id = OLD.c_program_user_id;
  
  -- Clear mentor_id in proposals where this user was the mentor
  UPDATE t_proposal 
  SET c_mentor_id = NULL 
  WHERE c_mentor_id = OLD.c_program_user_id;
  
  RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cleanup_ft_roles_trigger
BEFORE DELETE ON t_program_user
FOR EACH ROW EXECUTE FUNCTION cleanup_ft_roles_on_user_removal();

-- Trigger to clear reviewer and mentor when proposal type changes away from FT
CREATE OR REPLACE FUNCTION cleanup_ft_fields_on_type_change()
RETURNS TRIGGER AS $$
BEGIN
  -- Check if we're changing away from Fast Turnaround
  IF OLD.c_science_subtype = 'fast_turnaround' AND NEW.c_science_subtype != 'fast_turnaround' THEN
    -- Clear the reviewer and mentor fields in the proposal
    NEW.c_reviewer_id := NULL;
    NEW.c_mentor_id := NULL;
  END IF;
  
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cleanup_ft_fields_on_type_change_trigger
BEFORE UPDATE OF c_science_subtype ON t_proposal
FOR EACH ROW EXECUTE FUNCTION cleanup_ft_fields_on_type_change();