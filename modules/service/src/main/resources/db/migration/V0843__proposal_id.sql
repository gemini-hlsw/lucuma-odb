-- Add a semester year columnt to program.  We'll default to the current year
-- for any existing programs, but then remove the default going forward.

CREATE DOMAIN d_semester_year AS int CHECK (VALUE >= 1 AND VALUE <= 999999999); -- Year.MAX_VALUE

ALTER TABLE t_program
  ADD COLUMN c_semester_year d_semester_year;

-- Add a semester half (A/B) column to program.  We'll default to the current
-- semester for any existing programs, but then remove the default going forward.

CREATE TYPE e_semester_half AS ENUM ('A', 'B');
COMMENT ON TYPE e_semester_half IS 'Semester Half A [Feb - Aug), B [Aug - Feb)';

ALTER TABLE t_program
  ADD COLUMN c_semester_half e_semester_half;

-- Default value for existing programs.
UPDATE t_program
  SET
    c_semester_year =
        EXTRACT(YEAR FROM CURRENT_TIMESTAMP AT TIME ZONE 'UTC')::d_semester_year,
    c_semester_half =
        CASE
            WHEN EXTRACT(MONTH FROM CURRENT_TIMESTAMP AT TIME ZONE 'UTC') BETWEEN 2 AND 7 THEN 'A'::e_semester_half
            ELSE 'B'::e_semester_half
        END
  WHERE
    c_proposal_status <> 'not_submitted';

-- Every submitted proposal has been assigned a semester.
ALTER TABLE t_program
  ADD CONSTRAINT submitted_proposal_has_semester
  CHECK (c_proposal_status = 'not_submitted' OR (c_semester_year IS NOT NULL AND c_semester_half IS NOT NULL));

-- A function that formats a semester.
CREATE OR REPLACE FUNCTION format_semester(year_value d_semester_year, half_value e_semester_half)
RETURNS VARCHAR
AS $$
BEGIN
    RETURN CONCAT(LPAD(year_value::text, 4, '0'), UPPER(half_value::text));
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- A function that obtains the next proposal index for a semester
CREATE OR REPLACE FUNCTION next_proposal_index(year_value d_semester_year, half_value e_semester_half)
RETURNS INT
AS $$
DECLARE
    sequence_name VARCHAR := CONCAT('s_', format_semester(year_value, half_value));
BEGIN
    EXECUTE 'CREATE SEQUENCE IF NOT EXISTS ' || sequence_name;

    RETURN nextval(sequence_name)::INT;
END;
$$ LANGUAGE plpgsql;

-- Add a proposal index column
ALTER TABLE t_program
  ADD COLUMN c_semester_index int4 CHECK (c_semester_index > 0);

-- Set the proposal index for existing programs that have a semester (i.e., have
-- been submitted).
UPDATE t_program
  SET c_semester_index = next_proposal_index(c_semester_year, c_semester_half)
  WHERE c_semester_year IS NOT NULL AND c_semester_half IS NOT NULL;

-- Every submitted proposal has an index.
ALTER TABLE t_program
  ADD CONSTRAINT submitted_proposal_has_index
  CHECK (c_proposal_status = 'not_submitted' OR c_semester_index IS NOT NULL);

-- Add a trigger that (re)sets the proposal index when submitted or when the
-- semester changes.
CREATE OR REPLACE FUNCTION update_proposal_index()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.c_proposal_status <> 'not_submitted' AND (
        (NEW.c_semester_year IS NOT NULL AND NEW.c_semester_half IS NOT NULL AND NEW.c_semester_index IS NULL) OR
        (NEW.c_semester_year IS DISTINCT FROM OLD.c_semester_year OR NEW.c_semester_half IS DISTINCT FROM OLD.c_semester_half)
    ) THEN
        NEW.c_semester_index := next_proposal_index(NEW.c_semester_year, NEW.c_semester_half);

    ELSIF NEW.c_proposal_status = 'not_submitted' AND (
        NEW.c_semester_year IS NULL OR NEW.c_semester_half IS NULL
    ) THEN
        NEW.c_semester_index := NULL;

    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_proposal_index_trigger
BEFORE UPDATE ON t_program
FOR EACH ROW
EXECUTE FUNCTION update_proposal_index();

-- Add a generated column that formats the proposal reference from the semester
-- and index.
CREATE DOMAIN d_proposal_reference AS varchar
  CHECK (VALUE ~ '^G-[0-9][0-9][0-9][0-9]+[AB]-[0-9][0-9][0-9][0-9]+$');
COMMENT ON DOMAIN d_proposal_reference IS 'Formatted Proposal Reference';

CREATE OR REPLACE FUNCTION format_proposal_reference(year_value d_semester_year, half_value e_semester_half, index_value int4)
RETURNS VARCHAR
AS $$
BEGIN
    RETURN CASE
        WHEN year_value IS NULL OR half_value IS NULL OR index_value IS NULL THEN NULL
        ELSE CONCAT('G-', format_semester(year_value, half_value), '-', LPAD(index_value::text, 4, '0'))
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_program
  ADD COLUMN c_proposal_reference d_proposal_reference GENERATED ALWAYS AS (format_proposal_reference(c_semester_year, c_semester_half, c_semester_index)) STORED;