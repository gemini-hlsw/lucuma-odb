-- Add a flag to users marking an investigator as classical
ALTER TABLE t_program_user
  ADD COLUMN c_classical_observer boolean NOT NULL DEFAULT false;

DROP VIEW v_program_user;

CREATE VIEW v_program_user AS
SELECT
  p.*,
  COALESCE(p.c_preferred_email, u.c_orcid_email) AS c_email,
  COALESCE(
    user_profile_display_name(p.c_preferred_credit_name, p.c_preferred_given_name, p.c_preferred_family_name),
    user_profile_display_name(u.c_orcid_credit_name, u.c_orcid_given_name, u.c_orcid_family_name)
  ) AS c_display_name
FROM t_program_user p
LEFT JOIN t_user u ON p.c_user_id = u.c_user_id;
