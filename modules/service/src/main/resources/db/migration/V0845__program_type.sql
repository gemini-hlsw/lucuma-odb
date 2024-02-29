DROP TRIGGER update_semester_index_trigger ON t_program;

-- Instruments gain a new reference name used in program references.
ALTER TABLE t_instrument
  ADD COLUMN c_reference_name text;

UPDATE t_instrument
  SET c_reference_name =
    CASE
      WHEN c_tag = 'GmosNorth' THEN 'GMOSN'
      WHEN c_tag = 'GmosSouth' THEN 'GMOSS'
      ELSE UPPER(c_tag)
    END;

ALTER TABLE t_instrument
  ADD CONSTRAINT check_reference_name CHECK (
    c_reference_name IS NOT NULL AND c_reference_name ~ '^[A-Z0-9]+$'
  );

-- The broad categories of programs.  If a science program, then there is a
-- sub-type (see t_science_subtype below) that further distinguishes it.

CREATE TYPE e_program_type AS ENUM (
  'calibration',
  'engineering',
  'example',
  'library',
  'science'
 );
COMMENT ON TYPE e_program_type IS 'Broad classification of a program.';

CREATE TABLE t_program_type (
  c_type       e_program_type NOT NULL PRIMARY KEY,
  c_abbr       text           NOT NULL UNIQUE CHECK (c_abbr ~ '^[A-Z]{3}$'),
  c_name       text           NOT NULL,
  c_semesterly boolean        NOT NULL
);
COMMENT ON TABLE t_program_type IS 'Broad program classification definition.';

INSERT INTO t_program_type VALUES ('calibration', 'CAL', 'Calibration', true);
INSERT INTO t_program_type VALUES ('engineering', 'ENG', 'Engineering', true);
INSERT INTO t_program_type VALUES ('example',     'XPL', 'Example', false);
INSERT INTO t_program_type VALUES ('library',     'LIB', 'Library', false);
INSERT INTO t_program_type VALUES ('science',     'SCI', 'Science', true);



-- Create a type enumeration for science program types.

CREATE TYPE e_science_subtype AS ENUM (
  'classical',
  'directors_time',
  'fast_turnaround',
  'large_program',
  'queue',
  'demo_science',
  'system_verification'
);
COMMENT ON TYPE e_science_subtype IS 'Sub-type for science programs.';

CREATE TABLE t_science_subtype (
  c_type e_science_subtype NOT NULL PRIMARY KEY,
  c_abbr char              NOT NULL UNIQUE CHECK (c_abbr IN ('C', 'D', 'F', 'L', 'Q', 'S', 'V')),
  c_name text              NOT NULL
);
COMMENT ON TABLE t_science_subtype IS 'Science program sub-type definition.';

INSERT INTO t_science_subtype VALUES ('classical',           'C', 'Classical');
INSERT INTO t_science_subtype VALUES ('directors_time',      'D', 'Directors Time');
INSERT INTO t_science_subtype VALUES ('fast_turnaround',     'F', 'Fast Turnaround');
INSERT INTO t_science_subtype VALUES ('large_program',       'L', 'Large Program');
INSERT INTO t_science_subtype VALUES ('queue',               'Q', 'Queue');
INSERT INTO t_science_subtype VALUES ('demo_science',        'S', 'Demo Science');
INSERT INTO t_science_subtype VALUES ('system_verification', 'V', 'System Verification');



-- Add a program type and science type to the t_program table
ALTER TABLE t_program
  ADD COLUMN c_program_type    e_program_type    NOT NULL DEFAULT 'science' :: e_program_type,
  ADD COLUMN c_library_desc    text              NULL CHECK (c_library_desc ~ '^[A-Z0-9]+$'), -- LIB
  ADD COLUMN c_instrument      d_tag             NULL REFERENCES t_instrument(c_tag),         -- CAL, ENG, LIB, XPL
  ADD COLUMN c_science_subtype e_science_subtype NULL,                                        -- SCI

  ADD CONSTRAINT reference_type_constraint CHECK (
      CASE
          WHEN c_program_type = 'calibration' OR
               c_program_type = 'engineering' THEN
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


-- Set the initial science type from the proposal class, for accepted proposals.
UPDATE t_program AS g
    SET c_science_subtype = s.c_type
   FROM t_proposal p
   JOIN t_science_subtype s ON p.c_class = s.c_type :: d_tag
  WHERE p.c_program_id      = g.c_program_id
    AND g.c_program_type    = 'science'
    AND g.c_proposal_status = 'accepted';



-- A semester index applies to ENG, CAL, and SCI programs.  In the case of ENG
-- and CAL, the index is further distinguished by the associated instrument.

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

    IF program_type = 'calibration' OR program_type = 'engineering' THEN
        sequence_name := CONCAT(prefix, '_', instrument);
    ELSE
        sequence_name := prefix;
    END IF;

    EXECUTE 'CREATE SEQUENCE IF NOT EXISTS ' || sequence_name;
    RETURN nextval(sequence_name)::INT;
END;
$$ LANGUAGE plpgsql;

UPDATE t_program
  SET c_semester_index = next_semester_index(c_program_type, c_semester, c_instrument)
  WHERE c_semester IS NOT NULL;

CREATE OR REPLACE FUNCTION update_program_type()
RETURNS TRIGGER AS $$
BEGIN

    CASE
      WHEN NEW.c_program_type = 'calibration' OR
           NEW.c_program_type = 'engineering' THEN
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

-- Replace the old ON UPDATE trigger with one that fires on INSERT OR UPDATE.
CREATE TRIGGER update_program_type_trigger
BEFORE INSERT OR UPDATE ON t_program
FOR EACH ROW
EXECUTE FUNCTION update_program_type();


-- We want a "proposal reference" column and a "program reference" column, so
-- we need to transform the existing program reference column accordingly.

ALTER TABLE t_program
  DROP COLUMN c_program_reference;

DROP DOMAIN d_program_reference;

CREATE OR REPLACE FUNCTION format_proposal_reference(ptype e_program_type, semester d_semester, index int4)
RETURNS text AS $$
BEGIN
    RETURN CASE
        WHEN ptype <> 'science' OR semester IS NULL OR index IS NULL THEN NULL
        ELSE CONCAT('G-', semester, '-', LPAD(index::text, 4, '0'))
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION format_science_reference(semester d_semester, index int4, science_subtype e_science_subtype)
RETURNS text AS $$
DECLARE
  abbr char;
  proposal_ref text;
BEGIN
    SELECT c_abbr INTO abbr FROM t_science_subtype WHERE c_type = science_subtype;
    proposal_ref := format_proposal_reference('science', semester, index);

    RETURN CASE
        WHEN proposal_ref IS NULL OR abbr IS NULL THEN NULL
        ELSE CONCAT(proposal_ref, '-', abbr)
    END;
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

CREATE OR REPLACE FUNCTION format_lib_or_xpl_reference(ptype e_program_type, instrument d_tag, description text)
RETURNS text AS $$
DECLARE
  abbr text;
  inst text;
  prefix text;
BEGIN
    SELECT c_abbr           INTO abbr FROM t_program_type WHERE c_type = ptype;
    SELECT c_reference_name INTO inst FROM t_instrument   WHERE c_tag = instrument;
    prefix := CONCAT('G-', abbr, '-', inst);

    RETURN  CASE
      WHEN ptype = 'library' THEN CONCAT(prefix, '-', description)
      ELSE prefix
    END;
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
      WHEN ptype = 'calibration' OR
           ptype = 'engineering' THEN
          format_cal_or_eng_reference(ptype, semester, index, instrument)

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

ALTER TABLE t_program
  ADD COLUMN c_proposal_reference text GENERATED ALWAYS AS (format_proposal_reference(c_program_type, c_semester, c_semester_index)) STORED UNIQUE,
  ADD COLUMN c_program_reference  text GENERATED ALWAYS AS (format_program_reference(c_program_type, c_semester, c_semester_index, c_proposal_status, c_science_subtype, c_instrument, c_library_desc)) STORED UNIQUE;


-- Add a trigger such that when a proposal class is updated the science type is
-- set to match.

CREATE OR REPLACE FUNCTION update_science_subtype()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.c_class IS DISTINCT FROM OLD.c_class THEN
        UPDATE t_program AS p
            SET c_science_subtype = CASE
                                        WHEN p.c_program_type = 'science' THEN s.c_type
                                        ELSE NULL
                                    END
           FROM t_science_subtype s
          WHERE s.c_type :: d_tag = NEW.c_class
            AND p.c_program_id    = NEW.c_program_id;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_science_subtype_trigger
BEFORE INSERT OR UPDATE ON t_proposal
FOR EACH ROW
EXECUTE FUNCTION update_science_subtype();

-- Create views for the proposal and program references such that we remove
-- entries without a reference.  This makes mapping the reference as
-- optional in Proposal and Program possible.

CREATE VIEW v_proposal_reference AS
  SELECT
    c_program_id,
    c_semester,
    c_semester_index,
    c_proposal_reference
  FROM
    t_program
  WHERE
    c_proposal_reference IS NOT NULL
  ORDER BY c_program_id;

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