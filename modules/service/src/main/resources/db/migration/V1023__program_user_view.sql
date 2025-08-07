-- Add a view for t_program_user which coallesces the email and name fields from 
-- t_user and t_program_user.

CREATE FUNCTION user_profile_display_name(
  creditName text,
  givenName text,
  familyName text
)
RETURNS text AS $$
BEGIN
  RETURN COALESCE(creditName, givenName || ' ' || familyName, familyName, givenName);	
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE VIEW v_program_user AS
SELECT
  p.*,
  COALESCE(p.c_fallback_email, u.c_orcid_email) AS c_email,
  COALESCE(
    user_profile_display_name(p.c_fallback_credit_name, p.c_fallback_given_name, p.c_fallback_family_name),
    user_profile_display_name(u.c_orcid_credit_name, u.c_orcid_given_name, u.c_orcid_family_name)
  ) AS c_display_name
FROM t_program_user p
LEFT JOIN t_user u ON p.c_user_id = u.c_user_id;
