-- A proposal is one of three kinds, discriminated by c_observatory:
--   * a "Gemini" proposal (c_observatory = 'gemini'), which has a science subtype
--     (c_science_subtype) and requests time at Gemini; or
--   * an "external" proposal (c_observatory in 'keck'/'subaru'), in which a Gemini
--     PI requests time at another observatory via an exchange.  These have no
--     science subtype; Subaru proposals additionally carry a Subaru proposal type.
--
-- Every proposal has an observatory.  A science subtype is present if and only if
-- the observatory is Gemini.  A Subaru proposal type is present if and only if the
-- observatory is Subaru.
--
-- Separately, a Gemini queue or classical proposal whose PI belongs to an exchange
-- partner community (Keck/Subaru) carries that exchange partner in
-- c_exchange_partner; the entire time request is then assigned to it rather than
-- apportioned across Gemini partner splits.

ALTER TABLE t_proposal
  ADD COLUMN c_exchange_partner     e_exchange_partner     NULL,
  ADD COLUMN c_observatory          e_observatory          NOT NULL DEFAULT 'gemini',
  ADD COLUMN c_subaru_proposal_type e_subaru_proposal_type NULL;

-- Replace the science-subtype-only checks with a broader set of proposal-type
-- checks keyed off the observatory and (for Gemini) the science subtype.
DROP TRIGGER ch_proposal_science_subtype ON t_proposal;
DROP FUNCTION t_proposal_science_subtype_checks();

CREATE FUNCTION t_proposal_type_checks()
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

  -- Minimum percent time must be 0 for external proposals and Gemini poor weather.
  IF NEW.c_min_percent <> 0 AND ((NOT is_gemini) OR st = 'poor_weather') THEN
    RAISE EXCEPTION 'Minimum percent time must be 0 for this proposal type.';
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

CREATE CONSTRAINT TRIGGER ch_proposal_type
AFTER INSERT OR UPDATE ON t_proposal
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION t_proposal_type_checks();

-- Enforce the time-request invariant: a proposal cannot have both an exchange
-- partner (t_proposal.c_exchange_partner) and Gemini partner splits
-- (t_partner_split rows).  These live in different tables, so a CHECK cannot
-- express it.  Plain (immediate) row triggers raise during the offending
-- statement so the service can surface a clean error (a deferred constraint
-- trigger would instead fail at COMMIT, outside the request handler).
CREATE FUNCTION check_proposal_time_request() RETURNS trigger AS $$
DECLARE
  pid          d_program_id := COALESCE(NEW.c_program_id, OLD.c_program_id);
  has_exchange boolean;
BEGIN
  SELECT (c_exchange_partner IS NOT NULL) INTO has_exchange
    FROM t_proposal WHERE c_program_id = pid;

  IF has_exchange AND EXISTS (SELECT 1 FROM t_partner_split WHERE c_program_id = pid) THEN
    RAISE EXCEPTION 'A proposal may not have both an exchange partner and partner splits'
      USING ERRCODE = 'P0001';
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_proposal_time_request_proposal
  AFTER INSERT OR UPDATE ON t_proposal
  FOR EACH ROW EXECUTE FUNCTION check_proposal_time_request();

CREATE TRIGGER check_proposal_time_request_split
  AFTER INSERT OR UPDATE ON t_partner_split
  FOR EACH ROW EXECUTE FUNCTION check_proposal_time_request();

-- Recreate v_proposal to pick up the new columns and to add discriminant key
-- columns: one non-null only for each observatory (driving the
-- Gemini/Keck/Subaru proposal-type mappings), plus non-null placeholder columns
-- for the Gemini science subtype and Subaru call type used by those mappings.
DROP VIEW v_proposal;
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
    -- Non-null Subaru call type for the SubaruProposalType mapping.  Only
    -- meaningful for Subaru proposals; others get a placeholder that is never
    -- rendered (their c_program_id_subaru key is null).
    COALESCE(p.c_subaru_proposal_type, 'normal')                             AS c_subaru_proposal_type_d,
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

-- Mirror the observatory and Subaru proposal type onto t_program (as is done with
-- c_science_subtype) so the IMMUTABLE reference-formatting functions can build the
-- program reference without consulting t_proposal.
ALTER TABLE t_program
  ADD COLUMN c_observatory          e_observatory          NOT NULL DEFAULT 'gemini',
  ADD COLUMN c_subaru_proposal_type e_subaru_proposal_type NULL;

-- A submitted science program requires a science subtype only when it is a Gemini
-- proposal; external proposals have none.  Also clear the Subaru proposal type for
-- non-science program types.
CREATE OR REPLACE FUNCTION update_program_type()
RETURNS TRIGGER AS $$
BEGIN

    CASE
      WHEN NEW.c_program_type = 'calibration'   OR
           NEW.c_program_type = 'commissioning' OR
           NEW.c_program_type = 'engineering'   OR
           NEW.c_program_Type = 'monitoring'    THEN
        BEGIN
          IF NEW.c_semester IS NULL THEN
            RAISE EXCEPTION '% programs must define a semester', INITCAP(NEW.c_program_type);
          ELSEIF NEW.c_instrument IS NULL THEN
            RAISE EXCEPTION '% programs must define an instrument', INITCAP(NEW.c_program_type);
          ELSEIF (NEW.c_program_type != OLD.c_program_type)         OR
                 (NEW.c_semester   IS DISTINCT FROM OLD.c_semester)   OR
                 (NEW.c_instrument IS DISTINCT FROM OLD.c_instrument) THEN
            NEW.c_semester_index := next_semester_index(NEW.c_program_type, NEW.c_semester, NEW.c_instrument);
          END IF;
          NEW.c_library_desc          := NULL;
          NEW.c_science_subtype       := NULL;
          NEW.c_subaru_proposal_type  := NULL;
        END;

      WHEN NEW.c_program_type = 'example' THEN
        BEGIN
          IF NEW.c_instrument IS NULL THEN
            RAISE EXCEPTION 'Example programs must define an instrument';
          END IF;
          NEW.c_semester              := NULL;
          NEW.c_semester_index        := NULL;
          NEW.c_library_desc          := NULL;
          NEW.c_science_subtype       := NULL;
          NEW.c_subaru_proposal_type  := NULL;
        END;

      WHEN NEW.c_program_type = 'library' THEN
        BEGIN
          IF NEW.c_instrument IS NULL THEN
            RAISE EXCEPTION 'Library programs must define an instrument';
          ELSEIF NEW.c_library_desc IS NULL THEN
            RAISE EXCEPTION 'Library programs must define a description';
          END IF;
          NEW.c_semester              := NULL;
          NEW.c_semester_index        := NULL;
          NEW.c_science_subtype       := NULL;
          NEW.c_subaru_proposal_type  := NULL;
        END;

      WHEN NEW.c_program_type = 'science' THEN
        BEGIN
          IF NEW.c_proposal_status <> 'not_submitted' THEN
            -- A submitted science program must have a semester.  A Gemini proposal
            -- must also have a science subtype; an external (exchange) proposal has
            -- none.
            IF NEW.c_semester IS NULL THEN
              RAISE EXCEPTION 'Submitted science programs must define a semester';
            ELSEIF NEW.c_observatory = 'gemini' AND NEW.c_science_subtype IS NULL THEN
              RAISE EXCEPTION 'Submitted Gemini science programs must define a science subtype.';
            ELSEIF (NEW.c_program_type != OLD.c_program_type) OR
                   (NEW.c_semester IS DISTINCT FROM OLD.c_semester) OR
                   (OLD.c_proposal_status = 'not_submitted' AND NEW.c_semester_index IS NULL) THEN
              NEW.c_semester_index := next_semester_index('science', NEW.c_semester, NULL);
            END IF;
          ELSEIF (NEW.c_semester IS DISTINCT FROM OLD.c_semester) THEN
            -- Since it is not submitted and the semester has changed, we lose
            -- any index we may have had while previously submitted
            NEW.c_semester_index := NULL;
          END IF;
          NEW.c_instrument   := NULL;
          NEW.c_library_desc := NULL;
        END;

      WHEN NEW.c_program_type = 'system' THEN
        BEGIN
          IF NEW.c_library_desc IS NULL THEN
            RAISE EXCEPTION 'System programs must define a description';
          END IF;
          NEW.c_instrument            := NULL;
          NEW.c_semester              := NULL;
          NEW.c_semester_index        := NULL;
          NEW.c_science_subtype       := NULL;
          NEW.c_subaru_proposal_type  := NULL;
        END;

    END CASE;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- The program reference for science programs now depends on the observatory and
-- (for Subaru) the Subaru proposal type.  Drop the generated column so the
-- formatting functions can be replaced, then recreate it with the new inputs.
-- v_program_reference selects the generated column, and a trigger fires on
-- updates of it, so both must be dropped and recreated around the change.
DROP VIEW v_program_reference;
DROP TRIGGER update_program_reference_in_observation_trigger ON t_program;
ALTER TABLE t_program
  DROP COLUMN c_program_reference;

DROP FUNCTION format_program_reference(e_program_type, d_semester, int4, d_tag, e_science_subtype, d_tag, text);
DROP FUNCTION format_science_reference(d_semester, int4, e_science_subtype);

-- The trailing letter of a science program reference identifies the proposal
-- kind: for Gemini proposals it is the science-subtype abbreviation; for external
-- proposals it is 'K' (Keck), 'U' (Subaru) or 'I' (Subaru intensive).
CREATE FUNCTION format_science_reference(
  semester        d_semester,
  index           int4,
  observatory     e_observatory,
  science_subtype e_science_subtype,
  subaru_type     e_subaru_proposal_type
)
RETURNS text AS $$
DECLARE
  letter       char;
  proposal_ref text;
BEGIN
    letter := CASE
      WHEN observatory = 'keck'   THEN 'K'
      WHEN observatory = 'subaru' THEN CASE WHEN subaru_type = 'intensive' THEN 'I' ELSE 'U' END
      ELSE (SELECT c_abbr FROM t_science_subtype WHERE c_type = science_subtype)
    END;
    proposal_ref := format_proposal_reference('science', semester, index);

    RETURN CASE
        WHEN proposal_ref IS NULL OR letter IS NULL THEN NULL
        ELSE CONCAT(proposal_ref, '-', letter)
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE FUNCTION format_program_reference(
  ptype           e_program_type,
  semester        d_semester,
  index           int4,
  proposal_status d_tag,
  science_subtype e_science_subtype,
  instrument      d_tag,
  description     text,
  observatory     e_observatory,
  subaru_type     e_subaru_proposal_type
)
RETURNS text AS $$
BEGIN
    RETURN CASE
      WHEN ptype = 'calibration'   OR
           ptype = 'commissioning' OR
           ptype = 'engineering'   OR
           ptype = 'monitoring'    THEN
          public.format_semester_instrument_reference(ptype, semester, index, instrument)

      WHEN ptype = 'example' OR
           ptype = 'library' THEN
          public.format_lib_or_xpl_reference(ptype, instrument, description)

      WHEN ptype = 'system' THEN
          CONCAT('SYS-', description)

      WHEN ptype = 'science' AND proposal_status = 'accepted' THEN
          public.format_science_reference(semester, index, observatory, science_subtype, subaru_type)

      ELSE
          NULL
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_program
  ADD COLUMN c_program_reference text GENERATED ALWAYS AS (
    format_program_reference(
      c_program_type,
      c_semester,
      c_semester_index,
      c_proposal_status,
      c_science_subtype,
      c_instrument,
      c_library_desc,
      c_observatory,
      c_subaru_proposal_type
    )
  ) STORED UNIQUE;

-- Recreate the observation-reference sync trigger on the new generated column.
CREATE TRIGGER update_program_reference_in_observation_trigger
AFTER UPDATE OF c_program_reference ON t_program
FOR EACH ROW
EXECUTE FUNCTION update_program_reference_in_observation();

-- Recreate v_program_reference now that the generated column exists again.
CREATE VIEW v_program_reference AS
  SELECT
    c_program_id,
    c_program_type,
    c_library_desc,
    c_instrument,
    c_science_subtype,
    c_semester,
    c_semester_index,
    c_program_reference
  FROM
    t_program
  WHERE
    c_program_reference IS NOT NULL
  ORDER BY c_program_id;
