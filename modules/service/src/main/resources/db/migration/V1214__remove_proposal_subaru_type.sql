-- The Subaru proposal type is a property of the Call for Proposals
-- (t_cfp.c_subaru_proposal_type), not of the proposal.  A Subaru proposal must
-- reference a Subaru call, so the two always agreed; the proposal-side copy was
-- pure redundancy.  Remove it.
--
-- The denormalized mirror on t_program (c_subaru_proposal_type), which the
-- IMMUTABLE program-reference function reads to pick the trailing reference
-- letter (U = normal, I = intensive), is retained.  It is now fed from the
-- linked call rather than from the proposal.

-- Drop v_proposal (its SELECT p.* depends on the column), then drop the
-- redundant proposal-side Subaru type, then recreate the view.
DROP VIEW v_proposal;

-- Drop the redundant proposal-side Subaru type.
ALTER TABLE t_proposal
  DROP COLUMN c_subaru_proposal_type;

-- Recreate v_proposal (as in V1185) without the c_subaru_proposal_type_d
-- discriminator column, which was only used by the (now removed) SubaruProposalType
-- 'type' field mapping.  (The c_subaru_proposal_type column itself is gone, so
-- p.* no longer includes it.)
CREATE VIEW v_proposal AS
  SELECT
    p.*,
    CASE WHEN p.c_observatory = 'gemini'               THEN c_program_id END AS c_program_id_gemini,
    CASE WHEN p.c_observatory = 'keck'                 THEN c_program_id END AS c_program_id_keck,
    CASE WHEN p.c_observatory = 'subaru'               THEN c_program_id END AS c_program_id_subaru,
    -- Non-null discriminator for the GeminiProposalType interface mapping.  Only
    -- meaningful for Gemini proposals; others get a placeholder that is never
    -- rendered (their c_program_id_gemini key is null).
    COALESCE(p.c_science_subtype, 'queue')                                   AS c_gemini_science_subtype,
    CASE WHEN p.c_science_subtype = 'classical'           THEN c_program_id END AS c_program_id_c,
    CASE WHEN p.c_science_subtype = 'demo_science'        THEN c_program_id END AS c_program_id_s,
    CASE WHEN p.c_science_subtype = 'directors_time'      THEN c_program_id END AS c_program_id_d,
    CASE WHEN p.c_science_subtype = 'fast_turnaround'     THEN c_program_id END AS c_program_id_f,
    CASE WHEN p.c_science_subtype = 'large_program'       THEN c_program_id END AS c_program_id_l,
    CASE WHEN p.c_science_subtype = 'poor_weather'        THEN c_program_id END AS c_program_id_p,
    CASE WHEN p.c_science_subtype = 'queue'               THEN c_program_id END AS c_program_id_q,
    CASE WHEN p.c_science_subtype = 'system_verification' THEN c_program_id END AS c_program_id_v
  FROM
    t_proposal p;

-- Redefine t_proposal_type_checks() as in V1207 but without the "a Subaru
-- proposal type is present iff the observatory is Subaru" checks (the column no
-- longer exists).  The ch_proposal_type trigger references the function by name,
-- so it does not need to be recreated.
CREATE OR REPLACE FUNCTION t_proposal_type_checks()
RETURNS TRIGGER AS $$
DECLARE
  is_gemini boolean           := (NEW.c_observatory = 'gemini');
  st        e_science_subtype := NEW.c_science_subtype;
BEGIN
  -- A science subtype is present iff the proposal is a Gemini proposal.
  IF is_gemini AND st IS NULL THEN
    RAISE EXCEPTION 'Gemini proposals must define a science subtype.';
  END IF;
  IF (NOT is_gemini) AND st IS NOT NULL THEN
    RAISE EXCEPTION 'Only Gemini proposals may define a science subtype.';
  END IF;

  -- An exchange partner may only be set on Gemini queue or classical proposals.
  IF NEW.c_exchange_partner IS NOT NULL AND NOT (is_gemini AND st IN ('queue', 'classical')) THEN
    RAISE EXCEPTION 'An exchange partner may only be set on Gemini queue or classical proposals.';
  END IF;

  -- TOO activation must be None except for Gemini subtypes other than classical
  -- and poor weather.
  IF NEW.c_too_activation <> 'none' AND NOT (is_gemini AND st NOT IN ('classical', 'poor_weather')) THEN
    RAISE EXCEPTION 'TOO activation must be None for this proposal type.';
  END IF;

  -- Minimum percent time must be 0 for Gemini poor weather proposals.
  IF NEW.c_min_percent <> 0 AND is_gemini AND st = 'poor_weather' THEN
    RAISE EXCEPTION 'Minimum percent time must be 0 for Gemini poor weather proposals.';
  END IF;

  -- Total time and min percent total are set if and only if the proposal is a
  -- Gemini large program.
  IF (NEW.c_total_time IS NOT NULL OR NEW.c_min_percent_total IS NOT NULL)
       AND NOT (is_gemini AND st = 'large_program') THEN
    RAISE EXCEPTION 'Total time and min percent total may only be set on Gemini large programs.';
  END IF;
  IF is_gemini AND st = 'large_program'
       AND (NEW.c_total_time IS NULL OR NEW.c_min_percent_total IS NULL) THEN
    RAISE EXCEPTION 'Large Program proposals must define the total time and min percent total.';
  END IF;

  -- US long term may only be set on Gemini classical or queue proposals.
  IF NEW.c_us_long_term AND NOT (is_gemini AND st IN ('classical', 'queue')) THEN
    RAISE EXCEPTION 'US long term may only be set on Gemini classical or queue proposals.';
  END IF;

  -- AEON multi-facility and JWST synergy may only be set on Gemini classical,
  -- large program, or queue proposals.
  IF (NEW.c_aeon_multi_facility OR NEW.c_jwst_synergy)
       AND NOT (is_gemini AND st IN ('classical', 'large_program', 'queue')) THEN
    RAISE EXCEPTION 'AEON multi-facility and JWST synergy may only be set on Gemini classical, large program, or queue proposals.';
  END IF;

  -- Consider for band 3 may only be set on Gemini queue proposals.
  IF NEW.c_consider_for_band_3 <> 'unset' AND NOT (is_gemini AND st = 'queue') THEN
    RAISE EXCEPTION 'Consider for band 3 may only be set on Gemini queue proposals.';
  END IF;

  -- A mentor may only be set on Gemini fast turnaround proposals.
  IF NEW.c_mentor_id IS NOT NULL AND NOT (is_gemini AND st = 'fast_turnaround') THEN
    RAISE EXCEPTION 'A mentor may only be set on Gemini fast turnaround proposals.';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
