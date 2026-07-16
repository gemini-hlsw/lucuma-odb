-- Promote Keck/Subaru exchange proposals to first-class program types.
--
-- Previously an exchange proposal was a 'science' program discriminated by
-- t_program.c_observatory.  Now the program type itself is 'keck'/'subaru', the
-- reference machinery keys off c_program_type (+ c_subaru_proposal_type for the
-- Subaru U/I letter), and t_program.c_observatory is removed.  Reference labels
-- are unchanged: G-{semester}-{index}-{K|U|I}, with a single per-semester
-- proposal-number sequence shared across gemini/keck/subaru (so the proposal
-- reference G-YYYYs-NNNN stays globally unique and the trailing letter
-- distinguishes the type).

-- 1. program_type_abbr: add keck/subaru (used for sequence names; keck/subaru
--    share the 'science' sequence, so these are for completeness).
CREATE OR REPLACE FUNCTION program_type_abbr(
  program_type e_program_type
)
RETURNS text AS $$
BEGIN
  RETURN CASE
    WHEN program_type = 'calibration'   THEN  'CAL'
    WHEN program_type = 'engineering'   THEN  'ENG'
    WHEN program_type = 'example'       THEN  'XPL'
    WHEN program_type = 'library'       THEN  'LIB'
    WHEN program_type = 'science'       THEN  'SCI'
    WHEN program_type = 'commissioning' THEN  'COM'
    WHEN program_type = 'monitoring'    THEN  'MON'
    WHEN program_type = 'system'        THEN  'SYS'
    WHEN program_type = 'keck'          THEN  'KCK'
    WHEN program_type = 'subaru'        THEN  'SUB'
  END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- 2. format_proposal_reference: proposal references now exist for science, keck
--    and subaru (all proposal-bearing types).  Signature unchanged, so the
--    c_proposal_reference generated column keeps working.
CREATE OR REPLACE FUNCTION format_proposal_reference(ptype e_program_type, semester d_semester, index int4)
RETURNS text AS $$
BEGIN
    RETURN CASE
        WHEN ptype NOT IN ('science', 'keck', 'subaru') OR semester IS NULL OR index IS NULL THEN NULL
        ELSE CONCAT('G-', semester, '-', LPAD(index::text, 4, '0'))
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- 3. Drop the old constraint (re-added at the end with keck/subaru branches) so
--    the data migration below can run without tripping the science-only rule.
ALTER TABLE t_program
  DROP CONSTRAINT reference_type_constraint;

-- 4. Tear down the c_program_reference generated column and its dependents so its
--    formatter functions can be replaced (same drop/recreate dance as V1185).
DROP VIEW v_program_reference;
DROP TRIGGER update_program_reference_in_observation_trigger ON t_program;
DROP VIEW v_program;
ALTER TABLE t_program
  DROP COLUMN c_program_reference;

-- 5. Replace the reference formatters, keying the trailing letter on program type
--    instead of observatory (which is being removed from t_program).
DROP FUNCTION format_program_reference(e_program_type, d_semester, int4, d_tag, e_science_subtype, d_tag, text, e_observatory, e_subaru_proposal_type);
DROP FUNCTION format_science_reference(d_semester, int4, e_observatory, e_science_subtype, e_subaru_proposal_type);

-- The trailing letter: keck -> 'K', subaru -> 'I'/'U' (by proposal type),
-- gemini science -> the science-subtype abbreviation (C/D/F/L/Q/S/V).
CREATE FUNCTION format_science_reference(
  semester        d_semester,
  index           int4,
  program_type    e_program_type,
  science_subtype e_science_subtype,
  subaru_type     e_subaru_proposal_type
)
RETURNS text AS $$
DECLARE
  letter       char;
  proposal_ref text;
BEGIN
    letter := CASE
      WHEN program_type = 'keck'   THEN 'K'
      WHEN program_type = 'subaru' THEN CASE WHEN subaru_type = 'intensive' THEN 'I' ELSE 'U' END
      ELSE (SELECT c_abbr FROM t_science_subtype WHERE c_type = science_subtype)
    END;
    proposal_ref := format_proposal_reference(program_type, semester, index);

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

      WHEN ptype IN ('science', 'keck', 'subaru') AND proposal_status = 'accepted' THEN
          public.format_science_reference(semester, index, ptype, science_subtype, subaru_type)

      ELSE
          NULL
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- 6. update_program_type: science is Gemini-only again (requires a subtype when
--    submitted); keck/subaru are new proposal-bearing branches that share the
--    'science' index sequence and carry no science subtype (subaru additionally
--    requires its proposal type).  All c_observatory references are removed.
CREATE OR REPLACE FUNCTION update_program_type()
RETURNS TRIGGER AS $$
BEGIN

    CASE
      WHEN NEW.c_program_type = 'calibration'   OR
           NEW.c_program_type = 'commissioning' OR
           NEW.c_program_type = 'engineering'   OR
           NEW.c_program_type = 'monitoring'    THEN
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
            IF NEW.c_semester IS NULL THEN
              RAISE EXCEPTION 'Submitted science programs must define a semester';
            ELSEIF NEW.c_science_subtype IS NULL THEN
              RAISE EXCEPTION 'Submitted science programs must define a science subtype.';
            ELSEIF (NEW.c_program_type != OLD.c_program_type) OR
                   (NEW.c_semester IS DISTINCT FROM OLD.c_semester) OR
                   (OLD.c_proposal_status = 'not_submitted' AND NEW.c_semester_index IS NULL) THEN
              NEW.c_semester_index := next_semester_index('science', NEW.c_semester, NULL);
            END IF;
          ELSEIF (NEW.c_semester IS DISTINCT FROM OLD.c_semester) THEN
            NEW.c_semester_index := NULL;
          END IF;
          NEW.c_instrument           := NULL;
          NEW.c_library_desc         := NULL;
          NEW.c_subaru_proposal_type := NULL;
        END;

      WHEN NEW.c_program_type = 'keck' THEN
        BEGIN
          IF NEW.c_proposal_status <> 'not_submitted' THEN
            IF NEW.c_semester IS NULL THEN
              RAISE EXCEPTION 'Submitted Keck programs must define a semester';
            ELSEIF (NEW.c_program_type != OLD.c_program_type) OR
                   (NEW.c_semester IS DISTINCT FROM OLD.c_semester) OR
                   (OLD.c_proposal_status = 'not_submitted' AND NEW.c_semester_index IS NULL) THEN
              NEW.c_semester_index := next_semester_index('science', NEW.c_semester, NULL);
            END IF;
          ELSEIF (NEW.c_semester IS DISTINCT FROM OLD.c_semester) THEN
            NEW.c_semester_index := NULL;
          END IF;
          NEW.c_instrument           := NULL;
          NEW.c_library_desc         := NULL;
          NEW.c_science_subtype       := NULL;
          NEW.c_subaru_proposal_type := NULL;
        END;

      WHEN NEW.c_program_type = 'subaru' THEN
        BEGIN
          IF NEW.c_proposal_status <> 'not_submitted' THEN
            IF NEW.c_semester IS NULL THEN
              RAISE EXCEPTION 'Submitted Subaru programs must define a semester';
            ELSEIF NEW.c_subaru_proposal_type IS NULL THEN
              RAISE EXCEPTION 'Submitted Subaru programs must define a Subaru proposal type.';
            ELSEIF (NEW.c_program_type != OLD.c_program_type) OR
                   (NEW.c_semester IS DISTINCT FROM OLD.c_semester) OR
                   (OLD.c_proposal_status = 'not_submitted' AND NEW.c_semester_index IS NULL) THEN
              NEW.c_semester_index := next_semester_index('science', NEW.c_semester, NULL);
            END IF;
          ELSEIF (NEW.c_semester IS DISTINCT FROM OLD.c_semester) THEN
            NEW.c_semester_index := NULL;
          END IF;
          NEW.c_instrument      := NULL;
          NEW.c_library_desc    := NULL;
          NEW.c_science_subtype := NULL;
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

-- 7. Data migration: convert existing exchange programs (science + observatory)
--    into the new keck/subaru program types.  Labels are unchanged (the trailing
--    letter was already K/U/I), so the regenerated references won't collide.
UPDATE t_program
   SET c_program_type = c_observatory::text::e_program_type
 WHERE c_program_type = 'science'
   AND c_observatory IN ('keck', 'subaru');

-- 8. Drop the now-redundant program-level observatory discriminator.
ALTER TABLE t_program
  DROP COLUMN c_observatory;

-- 9. Recreate the c_program_reference generated column (observatory-less
--    formatter) and its dependents.
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
      c_subaru_proposal_type
    )
  ) STORED UNIQUE;

CREATE VIEW v_program AS
  SELECT p.*, COALESCE(rc.c_resource_count, 0) AS c_resource_count
  FROM t_program p
  LEFT JOIN t_program_resource_count rc ON rc.c_program_id = p.c_program_id;

CREATE TRIGGER update_program_reference_in_observation_trigger
AFTER UPDATE OF c_program_reference ON t_program
FOR EACH ROW
EXECUTE FUNCTION update_program_reference_in_observation();

-- v_program_reference now also exposes c_subaru_proposal_type so the GraphQL
-- mapping can render the Subaru reference's proposal type.
CREATE VIEW v_program_reference AS
  SELECT
    c_program_id,
    c_program_type,
    c_library_desc,
    c_instrument,
    c_science_subtype,
    c_subaru_proposal_type,
    c_semester,
    c_semester_index,
    c_program_reference
  FROM
    t_program
  WHERE
    c_program_reference IS NOT NULL
  ORDER BY c_program_id;

-- 10. Re-add reference_type_constraint with keck/subaru branches; science is
--     Gemini-only again.
ALTER TABLE t_program
  ADD CONSTRAINT reference_type_constraint CHECK (
      CASE
          WHEN c_program_type = 'calibration'   OR
               c_program_type = 'commissioning' OR
               c_program_type = 'engineering'   OR
               c_program_type = 'monitoring'    THEN
            c_instrument      IS NOT NULL AND
            c_library_desc    IS NULL     AND
            c_semester        IS NOT NULL AND
            c_semester_index  IS NOT NULL AND
            c_science_subtype IS NULL

          WHEN c_program_type = 'example' THEN
            c_instrument      IS NOT NULL AND
            c_library_desc    IS NULL     AND
            c_semester        IS NULL     AND
            c_semester_index  IS NULL     AND
            c_science_subtype IS NULL

          WHEN c_program_type = 'system' THEN
            c_instrument      IS NULL     AND
            c_library_desc    IS NOT NULL AND
            c_semester        IS NULL     AND
            c_semester_index  IS NULL     AND
            c_science_subtype IS NULL

          WHEN c_program_type = 'library' THEN
            c_instrument      IS NOT NULL AND
            c_library_desc    IS NOT NULL AND
            c_semester        IS NULL     AND
            c_semester_index  IS NULL     AND
            c_science_subtype IS NULL

          WHEN c_program_type = 'science' THEN
            c_instrument           IS NULL AND
            c_library_desc         IS NULL AND
            c_subaru_proposal_type IS NULL AND
            (c_proposal_status = 'not_submitted' OR (
              c_semester        IS NOT NULL AND
              c_semester_index  IS NOT NULL AND
              c_science_subtype IS NOT NULL
            ))

          WHEN c_program_type = 'keck' THEN
            c_instrument           IS NULL AND
            c_library_desc         IS NULL AND
            c_science_subtype      IS NULL AND
            c_subaru_proposal_type IS NULL AND
            (c_proposal_status = 'not_submitted' OR (
              c_semester       IS NOT NULL AND
              c_semester_index IS NOT NULL
            ))

          WHEN c_program_type = 'subaru' THEN
            c_instrument      IS NULL AND
            c_library_desc    IS NULL AND
            c_science_subtype IS NULL AND
            (c_proposal_status = 'not_submitted' OR (
              c_semester             IS NOT NULL AND
              c_semester_index       IS NOT NULL AND
              c_subaru_proposal_type IS NOT NULL
            ))
      END
  );
