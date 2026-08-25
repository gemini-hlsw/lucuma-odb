-- Targets of Opportunity in Subaru exchange proposals.
--
-- As in V1245, except that Subaru joins Gemini.  The subtype rule is unchanged
-- and still applies only to Gemini proposals, where a subtype is always present:
-- classical and poor weather may not have Targets of Opportunity.  Subaru
-- proposals have no subtype and no such restriction; Keck proposals may not have
-- Targets of Opportunity at all.
CREATE OR REPLACE FUNCTION too_activation_permitted(
  observatory e_observatory,
  subtype     e_science_subtype
) RETURNS boolean AS $$
  SELECT CASE
    WHEN observatory = 'gemini'
    THEN subtype IS NOT NULL AND subtype NOT IN ('classical', 'poor_weather')
    ELSE observatory = 'subaru'
  END;
$$ LANGUAGE sql IMMUTABLE;

COMMENT ON FUNCTION too_activation_permitted(e_observatory, e_science_subtype) IS
  'Whether a proposal of this kind may have Targets of Opportunity at all. '
  'False for Gemini classical and poor weather proposals, and for Keck, where '
  'none have been asked for. Subaru proposals may, and derive their ceiling '
  'from their observations like any other.';