-- Allow external (Keck/Subaru) exchange proposals to carry a non-zero minimum
-- percent time.  Previously c_min_percent had to be 0 for any non-Gemini
-- proposal; now it is only pinned to 0 for Gemini poor weather proposals.
--
-- Redefine t_proposal_type_checks() identically to V1185 except for the
-- minimum-percent-time rule.  The ch_proposal_type trigger references the
-- function by name, so it does not need to be recreated.

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

  -- A Subaru proposal type is present iff the observatory is Subaru.
  IF NEW.c_observatory = 'subaru' AND NEW.c_subaru_proposal_type IS NULL THEN
    RAISE EXCEPTION 'Subaru proposals must define a Subaru proposal type.';
  END IF;
  IF NEW.c_observatory <> 'subaru' AND NEW.c_subaru_proposal_type IS NOT NULL THEN
    RAISE EXCEPTION 'Only Subaru proposals may define a Subaru proposal type.';
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
