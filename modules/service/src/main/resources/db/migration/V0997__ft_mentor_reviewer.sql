-- Add reviewer and mentor references to proposal table
-- Each FT proposal can have designated reviewer and mentor
ALTER TABLE t_proposal
  ADD COLUMN c_reviewer_id d_program_user_id NULL DEFAULT NULL REFERENCES t_program_user(c_program_user_id) ON DELETE SET NULL,
  ADD COLUMN c_mentor_id d_program_user_id NULL DEFAULT NULL REFERENCES t_program_user(c_program_user_id) ON DELETE SET NULL;

-- Add constraint to prevent same user being both reviewer and mentor
ALTER TABLE t_proposal
ADD CONSTRAINT chk_reviewer_mentor_different
CHECK (c_reviewer_id IS NULL OR c_mentor_id IS NULL OR c_reviewer_id != c_mentor_id);

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
    CASE WHEN p.c_science_subtype = 'fast_turnaround' THEN
      (SELECT
        CASE WHEN COUNT(*) = 1 THEN MIN(ps.c_partner::d_tag) ELSE NULL::d_tag END
        FROM t_partner_split ps
       WHERE ps.c_program_id = p.c_program_id)
    END AS c_ft_partner
  FROM
    t_proposal p;

-- Trigger to clear reviewer and mentor when program user is removed
CREATE OR REPLACE FUNCTION cleanup_ft_roles_on_user_removal()
RETURNS TRIGGER AS $$
BEGIN
  UPDATE t_proposal
  SET c_reviewer_id = NULL
  WHERE c_reviewer_id = OLD.c_program_user_id;

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
  IF OLD.c_science_subtype = 'fast_turnaround' AND NEW.c_science_subtype != 'fast_turnaround' THEN
    NEW.c_reviewer_id := NULL;
    NEW.c_mentor_id := NULL;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cleanup_ft_fields_on_type_change_trigger
BEFORE UPDATE OF c_science_subtype ON t_proposal
FOR EACH ROW EXECUTE FUNCTION cleanup_ft_fields_on_type_change();
