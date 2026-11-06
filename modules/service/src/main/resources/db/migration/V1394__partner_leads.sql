-- Partner leads: for each partner from which a proposal requests time, the
-- investigator that partner contacts during ITAC.  The lead is marked on the
-- program user rather than on the partner split, because a lead must be
-- affiliated with the partner it leads.  The partner a user leads is therefore
-- their own affiliation, so an investigator leads at most one partner by
-- construction.  A non-partner PI counts as US for this purpose.
--
-- Which users are leads is maintained by the service (PartnerLeadService), by
-- the rules in lucuma-core's PartnerLeads, whenever the splits or the team
-- change.  The constraints below only guard the shape of the data.

ALTER TABLE t_program_user
  ADD COLUMN c_partner_lead boolean NOT NULL DEFAULT false;

-- Only an investigator with a Gemini partner, or a non-partner PI, may lead.
ALTER TABLE t_program_user
  ADD CONSTRAINT program_user_partner_lead_check CHECK (
    NOT c_partner_lead OR (
      (c_role IN ('pi', 'coi', 'coi_ro') AND c_partner_link = 'has_gemini_partner') OR
      (c_role = 'pi'                     AND c_partner_link = 'has_non_partner')
    )
  );

-- At most one lead per partner in a program.
CREATE UNIQUE INDEX i_program_user_partner_lead
  ON t_program_user (c_program_id, (COALESCE(c_gemini_partner, 'us'::d_tag)))
  WHERE c_partner_lead;

-- Recreate v_program_user so its `p.*` picks up c_partner_lead, and add the
-- partner the user leads, if any.
DROP VIEW v_program_user;

CREATE VIEW v_program_user AS
SELECT
  p.*,
  COALESCE(p.c_preferred_email, u.c_orcid_email) AS c_email,
  COALESCE(
    user_profile_display_name(p.c_preferred_credit_name, p.c_preferred_given_name, p.c_preferred_family_name),
    user_profile_display_name(u.c_orcid_credit_name, u.c_orcid_given_name, u.c_orcid_family_name)
  ) AS c_display_name,
  CASE WHEN p.c_partner_lead THEN COALESCE(p.c_gemini_partner, 'us'::d_tag) END AS c_lead_partner
FROM t_program_user p
LEFT JOIN t_user u ON p.c_user_id = u.c_user_id;

-- Partner splits with the lead of each partner, if assigned.
CREATE VIEW v_partner_split AS
SELECT
  s.*,
  pu.c_program_user_id AS c_lead_id
FROM t_partner_split s
LEFT JOIN t_program_user pu
  ON  pu.c_program_id = s.c_program_id
  AND pu.c_partner_lead
  AND COALESCE(pu.c_gemini_partner, 'us'::d_tag) = s.c_partner;

-- Existing proposals: assign the leads that need no choice -- the PI for the
-- PI's partner, and otherwise the only eligible investigator.  This is what
-- PartnerLeads.normalize gives when no lead is yet assigned.
WITH requested AS (
  SELECT s.c_program_id, s.c_partner
  FROM t_partner_split s
  JOIN t_proposal prop ON prop.c_program_id = s.c_program_id
  WHERE s.c_percent > 0
), eligible AS (
  SELECT
    r.c_program_id,
    r.c_partner,
    pu.c_program_user_id,
    pu.c_role
  FROM requested r
  JOIN t_program_user pu
    ON pu.c_program_id = r.c_program_id
   AND (
     (pu.c_role IN ('pi', 'coi', 'coi_ro') AND pu.c_partner_link = 'has_gemini_partner' AND pu.c_gemini_partner = r.c_partner) OR
     (pu.c_role = 'pi' AND pu.c_partner_link = 'has_non_partner' AND r.c_partner = 'us')
   )
), chosen AS (
  SELECT
    c_program_id,
    c_partner,
    COALESCE(
      MAX(c_program_user_id) FILTER (WHERE c_role = 'pi'),
      CASE WHEN COUNT(*) = 1 THEN MAX(c_program_user_id) END
    ) AS c_program_user_id
  FROM eligible
  GROUP BY c_program_id, c_partner
)
UPDATE t_program_user pu
   SET c_partner_lead = true
  FROM chosen c
 WHERE c.c_program_user_id = pu.c_program_user_id;
