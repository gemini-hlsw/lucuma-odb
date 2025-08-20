-- Rename the fallback profile columns to preferred profile columns on t_program_user
DROP VIEW v_program_user;

ALTER TABLE t_program_user
  RENAME COLUMN c_fallback_given_name TO c_preferred_given_name;

ALTER TABLE t_program_user
  RENAME COLUMN c_fallback_family_name TO c_preferred_family_name;

ALTER TABLE t_program_user
  RENAME COLUMN c_fallback_credit_name TO c_preferred_credit_name;

ALTER TABLE t_program_user
  RENAME COLUMN c_fallback_email TO c_preferred_email;

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
