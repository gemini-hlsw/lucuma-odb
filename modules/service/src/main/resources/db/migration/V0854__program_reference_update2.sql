-- Describe the new types in t_program_type
INSERT INTO t_program_type VALUES ('commissioning', 'COM', 'Commissioning', true);
INSERT INTO t_program_type VALUES ('monitoring',    'MON', 'Monitoring',    true);

-- Update the table constraint on reference types.
ALTER TABLE t_program
  DROP CONSTRAINT reference_type_constraint;

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

          WHEN c_program_type = 'library' THEN
            c_instrument      IS NOT NULL AND
            c_library_desc    IS NOT NULL AND
            c_semester        IS NULL     AND
            c_semester_index  IS NULL     AND
            c_science_subtype IS NULL

          WHEN c_program_type = 'science' THEN
            c_instrument   IS NULL     AND
            c_library_desc IS NULL     AND
            (c_proposal_status = 'not_submitted' OR (
              c_semester        IS NOT NULL AND
              c_semester_index  IS NOT NULL AND
              c_science_subtype IS NOT NULL
            ))
      END
  );

-- Update the next_semester_index function so that COM and MON are handled.
CREATE OR REPLACE FUNCTION next_semester_index(
  program_type e_program_type,
  semester     d_semester,
  instrument   d_tag
)
RETURNS INT AS $$
DECLARE
    abbr text;
    prefix text;
    sequence_name VARCHAR;
BEGIN
    SELECT c_abbr INTO abbr FROM t_program_type WHERE c_type = program_type;
    prefix := CONCAT('s_', semester, '_', abbr);

    IF instrument IS NOT NULL THEN
        sequence_name := CONCAT(prefix, '_', instrument);
    ELSE
        sequence_name := prefix;
    END IF;

    EXECUTE 'CREATE SEQUENCE IF NOT EXISTS ' || sequence_name;
    RETURN nextval(sequence_name)::INT;
END;
$$ LANGUAGE plpgsql;

-- Update the update_program_type function to handle COM and MON
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
          NEW.c_library_desc    := NULL;
          NEW.c_science_subtype := NULL;
        END;

      WHEN NEW.c_program_type = 'example' THEN
        BEGIN
          IF NEW.c_instrument IS NULL THEN
            RAISE EXCEPTION 'Example programs must define an instrument';
          END IF;
          NEW.c_semester        := NULL;
          NEW.c_semester_index  := NULL;
          NEW.c_library_desc    := NULL;
          NEW.c_science_subtype := NULL;
        END;

      WHEN NEW.c_program_type = 'library' THEN
        BEGIN
          IF NEW.c_instrument IS NULL THEN
            RAISE EXCEPTION 'Library programs must define an instrument';
          ELSEIF NEW.c_library_desc IS NULL THEN
            RAISE EXCEPTION 'Library programs must define a description';
          END IF;
          NEW.c_semester        := NULL;
          NEW.c_semester_index  := NULL;
          NEW.c_science_subtype := NULL;
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
          END IF;
          NEW.c_instrument   := NULL;
          NEW.c_library_desc := NULL;
        END;
    END CASE;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION format_semester_instrument_reference(ptype e_program_type, semester d_semester, index int4, instrument d_tag)
RETURNS text AS $$
DECLARE
  abbr text;
  inst text;
BEGIN
    SELECT c_abbr           INTO abbr FROM t_program_type WHERE c_type = ptype;
    SELECT c_reference_name INTO inst FROM t_instrument   WHERE c_tag = instrument;
    RETURN CONCAT('G-', semester, '-', abbr, '-', inst, '-', LPAD(index::text, 2, '0'));
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION format_cal_or_eng_reference(ptype e_program_type, semester d_semester, index int4, instrument d_tag)
RETURNS text AS $$
DECLARE
  abbr text;
  inst text;
BEGIN
    SELECT c_abbr           INTO abbr FROM t_program_type WHERE c_type = ptype;
    SELECT c_reference_name INTO inst FROM t_instrument   WHERE c_tag = instrument;
    RETURN CONCAT('G-', semester, '-', abbr, '-', inst, '-', LPAD(index::text, 2, '0'));
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION format_program_reference(
  ptype           e_program_type,
  semester        d_semester,
  index           int4,
  proposal_status d_tag,
  science_subtype e_science_subtype,
  instrument      d_tag,
  description     text
)
RETURNS text AS $$
BEGIN
    RETURN CASE
      WHEN ptype = 'calibration'   OR
           ptype = 'commissioning' OR
           ptype = 'engineering'   OR
           ptype = 'monitoring'    THEN
          format_semester_instrument_reference(ptype, semester, index, instrument)

      WHEN ptype = 'example' OR
           ptype = 'library' THEN
          format_lib_or_xpl_reference(ptype, instrument, description)

      WHEN ptype = 'science' AND proposal_status = 'accepted' THEN
          format_science_reference(semester, index, science_subtype)

      ELSE
          NULL
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

DROP FUNCTION format_cal_or_eng_reference;