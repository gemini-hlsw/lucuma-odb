-- In this migration we will drop the t_program_type table.  It accompanied
-- the e_program_type enumeration, adding additional information about each
-- enum value.  The problem is that the information in this table was used for
-- formatting program references in the generated c_program_reference column of
-- t_program.  This works normally, but the postgres backup writes the
-- program entries *before* the t_program_type entries. Upon restore, it
-- computes the program references with a NULL program type abbreviation because
-- the t_program_type table is still empty when needed.  Postgres has no idea
-- that there is a dependency.

-- The solution is to replace the t_program_type table with a simple
-- immutable function that returns an abbreviation for each type.

CREATE OR REPLACE FUNCTION program_type_abbr(
  program_type e_program_type
)
RETURNS text AS $$
BEGIN
  RETURN CASE
    WHEN program_type = 'calibration'   THEN  'CAL'
    WHEN program_type = 'engineering'   THEN  'ENG'
    WHEN program_type = 'example'       THEN  'XPL'
    when program_type = 'library'       THEN  'LIB'
    when program_type = 'science'       THEN  'SCI'
    when program_type = 'commissioning' THEN  'COM'
    when program_type = 'monitoring'    THEN  'MON'
    when program_type = 'system'        THEN  'SYS'
  END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

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
    abbr   := program_type_abbr(program_type);
    prefix := CONCAT('s_', semester, '_', abbr);

    IF instrument IS NOT NULL THEN
        sequence_name := CONCAT(prefix, '_', instrument);
    ELSE
        sequence_name := prefix;
    END IF;

    BEGIN
        EXECUTE 'CREATE SEQUENCE IF NOT EXISTS ' || sequence_name;
    EXCEPTION WHEN unique_violation THEN
        NULL; -- sequence exists, this was an attempt to create it in parallel
    END;

    RETURN nextval(sequence_name)::INT;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION format_semester_instrument_reference(ptype e_program_type, semester d_semester, index int4, instrument d_tag)
RETURNS text AS $$
DECLARE
  abbr text;
  inst text;
BEGIN
    SELECT c_reference_name INTO inst FROM t_instrument   WHERE c_tag = instrument;
    abbr   := program_type_abbr(ptype);
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
    SELECT c_reference_name INTO inst FROM t_instrument   WHERE c_tag = instrument;
    abbr   := program_type_abbr(ptype);
    prefix := CONCAT('G-', abbr, '-', inst);

    RETURN  CASE
      WHEN ptype = 'library' THEN CONCAT(prefix, '-', description)
      ELSE prefix
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_program
  DROP CONSTRAINT t_program_c_program_type_fkey;

DROP TABLE t_program_type;