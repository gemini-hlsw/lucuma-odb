-- The user id repeated only when the role grants write access, so the GraphQL
-- write predicate can test user and role on the same program_user row.  Must
-- stay in sync with existsUserWriteAccess.
CREATE OR REPLACE VIEW v_program_user AS
SELECT
  p.*,
  COALESCE(p.c_preferred_email, u.c_orcid_email) AS c_email,
  COALESCE(
    user_profile_display_name(p.c_preferred_credit_name, p.c_preferred_given_name, p.c_preferred_family_name),
    user_profile_display_name(u.c_orcid_credit_name, u.c_orcid_given_name, u.c_orcid_family_name)
  ) AS c_display_name,
  CASE
    WHEN p.c_role IN ('pi', 'coi', 'support_primary', 'support_secondary') THEN p.c_user_id
  END AS c_writer_user_id
FROM t_program_user p
LEFT JOIN t_user u ON p.c_user_id = u.c_user_id;
