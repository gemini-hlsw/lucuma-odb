-- Add a semester column to program.  It is nullable but there will be a
-- constraint that checks that it is set when the proposal status is not
-- 'not_submitted'.

CREATE DOMAIN d_semester AS varchar
  CHECK (VALUE ~ '^[0-9][0-9][0-9][0-9]+[AB]$');
COMMENT ON DOMAIN d_semester IS 'Formatted Semester';

ALTER TABLE t_program
  ADD COLUMN c_semester d_semester;

-- Default semester for existing submitted programs.
UPDATE t_program
  SET
    c_semester =
        CONCAT(
            LPAD(EXTRACT(YEAR FROM CURRENT_TIMESTAMP AT TIME ZONE 'UTC')::text, 4, '0'),
            CASE
                WHEN EXTRACT(MONTH FROM CURRENT_TIMESTAMP AT TIME ZONE 'UTC') BETWEEN 2 AND 7 THEN 'A'
                ELSE 'B'
            END
        )::d_semester
  WHERE
    c_proposal_status <> 'not_submitted';

-- Every submitted proposal has been assigned a semester.
--ALTER TABLE t_program
--  ADD CONSTRAINT submitted_proposal_has_semester
--  CHECK (c_proposal_status = 'not_submitted' OR c_semester IS NOT NULL);

-- A function that obtains the next proposal index for a semester
CREATE OR REPLACE FUNCTION next_proposal_index(semester d_semester)
RETURNS INT
AS $$
DECLARE
    sequence_name VARCHAR := CONCAT('s_', semester);
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
  SET c_semester_index = next_proposal_index(c_semester)
  WHERE c_semester IS NOT NULL;

-- Every submitted proposal has an index.
--ALTER TABLE t_program
--  ADD CONSTRAINT submitted_proposal_has_index
--  CHECK (c_proposal_status = 'not_submitted' OR c_semester_index IS NOT NULL);

-- Add a trigger that (re)sets the proposal index when submitted or when the
-- semester changes.
CREATE OR REPLACE FUNCTION update_proposal_index()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.c_proposal_status <> 'not_submitted'::d_tag THEN
        IF NEW.c_semester IS NULL THEN
            RAISE EXCEPTION 'Submitted proposals must have a semester';
        ELSEIF NEW.c_semester_index IS NULL OR NEW.c_semester IS DISTINCT FROM OLD.c_semester THEN
            NEW.c_semester_index := next_proposal_index(NEW.c_semester);
        END IF;
    ELSEIF NEW.c_proposal_status = 'not_submitted'::d_tag AND NEW.c_semester IS NULL THEN
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
CREATE DOMAIN d_program_reference AS varchar
  CHECK (VALUE ~ '^G-[0-9][0-9][0-9][0-9]+[AB]-[0-9][0-9][0-9][0-9]+(-[CDFLQSV])?$');
COMMENT ON DOMAIN d_program_reference IS 'Formatted Program Reference';

CREATE OR REPLACE FUNCTION format_program_reference(semester d_semester, index_value int4)
RETURNS VARCHAR
AS $$
BEGIN
    RETURN CASE
        WHEN semester IS NULL OR index_value IS NULL THEN NULL
        ELSE CONCAT('G-', semester, '-', LPAD(index_value::text, 4, '0'))
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_program
  ADD COLUMN c_program_reference d_program_reference GENERATED ALWAYS AS (format_program_reference(c_semester, c_semester_index)) STORED UNIQUE;

-- An index on the proposal reference to facilitate lookup
--CREATE INDEX program_reference_index ON t_program (c_program_reference);