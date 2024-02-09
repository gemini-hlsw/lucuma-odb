DROP TRIGGER update_semester_index_trigger ON t_program;

-- The broad categories of programs.  If a science program, then there is a
-- sub-type (see t_science_type below) that further distinguishes it.

CREATE TYPE e_program_class AS ENUM (
  'calibration',
  'engineering',
  'example',
  'library',
  'science'
 );
COMMENT ON TYPE e_program_class IS 'Broad classification of a program.';

CREATE TABLE t_program_class (
  c_class      e_program_class NOT NULL PRIMARY KEY,
  c_abbr       text            NOT NULL UNIQUE CHECK (c_abbr ~ '^[A-Z]{3}$'),
  c_name       text            NOT NULL,
  c_semesterly boolean         NOT NULL
);
COMMENT ON TABLE t_program_class IS 'Broad program classification definition.';

INSERT INTO t_program_class VALUES ('calibration', 'CAL', 'Calibration', true);
INSERT INTO t_program_class VALUES ('engineering', 'ENG', 'Engineering', true);
INSERT INTO t_program_class VALUES ('example',     'XPL', 'Example', false);
INSERT INTO t_program_class VALUES ('library',     'LIB', 'Library', false);
INSERT INTO t_program_class VALUES ('science',     'SCI', 'Science', true);



-- Create a type enumeration for science program types.

CREATE TYPE e_science_type AS ENUM (
  'classical',
  'directors_time',
  'fast_turnaround',
  'large_program',
  'queue',
  'demo_science',
  'system_verification'
);
COMMENT ON TYPE e_science_type IS 'Science program sub-type.';

CREATE TABLE t_science_type (
  c_type e_science_type  NOT NULL PRIMARY KEY,
  c_abbr char            NOT NULL UNIQUE CHECK (c_abbr IN ('C', 'D', 'F', 'L', 'Q', 'S', 'V')),
  c_name text            NOT NULL
);
COMMENT ON TABLE t_science_type IS 'Science program sub-type definition.';

INSERT INTO t_science_type VALUES ('classical',           'C', 'Classical');
INSERT INTO t_science_type VALUES ('directors_time',      'D', 'Directors Time');
INSERT INTO t_science_type VALUES ('fast_turnaround',     'F', 'Fast Turnaround');
INSERT INTO t_science_type VALUES ('large_program',       'L', 'Large Program');
INSERT INTO t_science_type VALUES ('queue',               'Q', 'Queue');
INSERT INTO t_science_type VALUES ('demo_science',        'S', 'Demo Science');
INSERT INTO t_science_type VALUES ('system_verification', 'V', 'System Verification');



-- Add a program class and science type to the t_program table
ALTER TABLE t_program
  ADD COLUMN c_program_class e_program_class NOT NULL DEFAULT 'science',
  ADD COLUMN c_library_desc  text            NULL CHECK (c_library_desc ~ '^[A-Z0-9]+$'), -- LIB
  ADD COLUMN c_instrument    d_tag           NULL REFERENCES t_instrument(c_tag),         -- CAL, ENG, LIB, XPL
  ADD COLUMN c_science_type  e_science_type  NULL DEFAULT 'queue',                        -- SCI

  ADD CONSTRAINT reference_type_constraint CHECK (
      CASE
          WHEN c_program_class = 'calibration' OR
               c_program_class = 'engineering' THEN
            c_instrument   IS NOT NULL AND
            c_library_desc IS NULL     AND
            c_semester     IS NOT NULL AND
            c_science_type IS NULL

          WHEN c_program_class = 'example' THEN
            c_instrument   IS NOT NULL AND
            c_library_desc IS NULL     AND
            c_semester     IS NULL     AND
            c_science_type IS NULL

          WHEN c_program_class = 'library' THEN
            c_instrument   IS NOT NULL AND
            c_library_desc IS NOT NULL AND
            c_semester     IS NULL     AND
            c_science_type IS NULL

          WHEN c_program_class = 'science' THEN
            c_instrument   IS NULL     AND
            c_library_desc IS NULL     AND
            (c_semester IS NOT NULL OR c_proposal_status = 'not_submitted') AND
            c_science_type IS NOT NULL
      END
  );


-- Set the initial science type from the proposal class, for accepted proposals.
UPDATE t_program AS g
    SET c_science_type = s.c_type
   FROM t_proposal p
   JOIN t_science_type s ON p.c_class = s.c_type :: d_tag
  WHERE p.c_program_id      = g.c_program_id
    AND g.c_program_class   = 'science'
    AND g.c_proposal_status = 'accepted';



-- A semester index applies to ENG, CAL, and SCI programs.  In the case of ENG
-- and CAL, the index is further distinguished by the associated instrument.

CREATE OR REPLACE FUNCTION next_semester_index(
  program_class e_program_class,
  semester      d_semester,
  instrument    d_tag
)
RETURNS INT AS $$
DECLARE
    abbr VARCHAR;
    prefix VARCHAR;
    sequence_name VARCHAR;
BEGIN
    SELECT c_abbr INTO abbr FROM t_program_class WHERE c_class = program_class;
    prefix := CONCAT('s_', semester, '_', abbr);

    IF program_class = 'calibration' OR program_class = 'engineering' THEN
        sequence_name := CONCAT(prefix, '_', instrument);
    ELSE
        sequence_name := prefix;
    END IF;

    EXECUTE 'CREATE SEQUENCE IF NOT EXISTS ' || sequence_name;
    RETURN nextval(sequence_name)::INT;
END;
$$ LANGUAGE plpgsql;

UPDATE t_program
  SET c_semester_index = next_semester_index(c_program_class, c_semester, c_instrument)
  WHERE c_semester IS NOT NULL;

CREATE OR REPLACE FUNCTION update_semester_index()
RETURNS TRIGGER AS $$
DECLARE
    semesterly boolean;
    requires_semester boolean;
    was_relevant_update boolean;
BEGIN

    -- Does the reference associated with the proposal class contain a semester?
    SELECT c_semesterly INTO semesterly FROM t_program_class WHERE c_class = NEW.c_program_class;

    -- Do we require the semester to be defined in this case?
    requires_semester := semesterly AND (
      NEW.c_program_class <> 'science' OR NEW.c_proposal_status <> 'not_submitted'
    );

    -- Was there a relevant update?
    was_relevant_update :=
      (NEW.c_semester      IS DISTINCT FROM OLD.c_semester)      OR
      (NEW.c_program_class IS DISTINCT FROM OLD.c_program_class) OR
      (NEW.c_instrument    IS DISTINCT FROM OLD.c_instrument)    OR
      (NEW.c_proposal_status <> 'not_submitted' AND OLD.c_proposal_status = 'not_submitted' AND NEW.c_semester_index IS NULL);

    -- Update the semester index if necessary.
    IF NOT semesterly THEN
        NEW.c_semester_index := NULL;
    ELSEIF requires_semester THEN
        IF NEW.c_semester IS NULL THEN
            RAISE EXCEPTION 'Submitted science proposals, calibrations and engineering programs must define a semester';
        ELSEIF was_relevant_update THEN
            NEW.c_semester_index := next_semester_index(NEW.c_program_class, NEW.c_semester, NEW.c_instrument);
        END IF;
    ELSEIF NEW.c_semester IS NULL THEN
        NEW.c_semester_index := NULL;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Replace the old ON UPDATE trigger with one that fires on INSERT OR UPDATE.
CREATE TRIGGER update_semester_index_trigger
BEFORE INSERT OR UPDATE ON t_program
FOR EACH ROW
EXECUTE FUNCTION update_semester_index();


-- We want a "proposal reference" column and a "program reference" column, so
-- we need to transform the existing program reference column accordingly.

ALTER TABLE t_program
  DROP COLUMN c_program_reference;

DROP DOMAIN d_program_reference;

CREATE OR REPLACE FUNCTION format_proposal_reference(class e_program_class, semester d_semester, index int4)
RETURNS text AS $$
BEGIN
    RETURN CASE
        WHEN class <> 'science' OR semester IS NULL OR index IS NULL THEN NULL
        ELSE CONCAT('G-', semester, '-', LPAD(index::text, 4, '0'))
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION format_science_reference(semester d_semester, index int4, science_type e_science_type)
RETURNS text AS $$
DECLARE
  abbr char;
  proposal_ref text;
BEGIN
    SELECT c_abbr INTO abbr FROM t_science_type WHERE c_type = science_type;
    proposal_ref := format_proposal_reference('science', semester, index);

    RETURN CASE
        WHEN proposal_ref IS NULL OR abbr IS NULL THEN NULL
        ELSE CONCAT(proposal_ref, '-', abbr)
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION format_cal_or_eng_reference(class e_program_class, semester d_semester, index int4, instrument d_tag)
RETURNS text AS $$
DECLARE
  abbr char;
BEGIN
    SELECT c_abbr INTO abbr FROM t_program_class WHERE c_class = class;
    RETURN CONCAT('G-', semester, '-', abbr, '-', instrument, '-', LPAD(index::text, 2, '0'));
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION format_lib_or_xpl_reference(class e_program_class, instrument d_tag, description text)
RETURNS text AS $$
DECLARE
  abbr char;
  prefix text;
BEGIN
    SELECT c_abbr INTO abbr FROM t_program_class WHERE c_class = class;
    prefix := CONCAT('G-', abbr, '-', instrument);

    RETURN  CASE
      WHEN class = 'library' THEN CONCAT(prefix, '_', description)
      ELSE prefix
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION format_program_reference(
  class           e_program_class,
  semester        d_semester,
  index           int4,
  proposal_status d_tag,
  science_type    e_science_type,
  instrument      d_tag,
  description     text
)
RETURNS text AS $$
BEGIN
    RETURN CASE
      WHEN class = 'calibration' OR
           class = 'engineering' THEN
          format_cal_or_eng_reference(class, semester, index, instrument)

      WHEN class = 'example' OR
           class = 'library' THEN
          format_lib_or_xpl_reference(class, instrument, description)

      WHEN class = 'science' AND proposal_status = 'accepted' THEN
          format_science_reference(semester, index, science_type)

      ELSE
          null
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_program
  ADD COLUMN c_proposal_reference text GENERATED ALWAYS AS (format_proposal_reference(c_program_class, c_semester, c_semester_index)) STORED UNIQUE,
  ADD COLUMN c_program_reference  text GENERATED ALWAYS AS (format_program_reference(c_program_class, c_semester, c_semester_index, c_proposal_status, c_science_type, c_instrument, c_library_desc)) STORED UNIQUE;


-- Add a trigger such that when a proposal class is updated the science type is
-- set to match.

CREATE OR REPLACE FUNCTION update_science_type()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.c_class != OLD.c_class THEN
        UPDATE t_program AS p
            SET c_science_type = CASE
                                     WHEN p.c_program_class = 'science' THEN COALESCE(s.c_type, 'queue')
                                     ELSE NULL
                                 END
           FROM t_science_type s
          WHERE s.c_type :: d_tag = NEW.c_class
            AND p.c_program_id    = NEW.c_program_id;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_science_type_trigger
BEFORE UPDATE ON t_proposal
FOR EACH ROW
EXECUTE FUNCTION update_science_type();