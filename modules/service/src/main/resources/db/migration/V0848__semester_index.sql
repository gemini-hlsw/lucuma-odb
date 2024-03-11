-- Remove no longer used version from V0843__program_reference.sql.
DROP FUNCTION next_semester_index(d_semester);

-- Update the next semester index function to handle parallel creation
-- uniqueness constraint errors as in V0847__observation_reference.sql
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

    BEGIN
        EXECUTE 'CREATE SEQUENCE IF NOT EXISTS ' || sequence_name;
    EXCEPTION WHEN unique_violation THEN
        NULL; -- sequence exists, this was an attempt to create it in parallel
    END;

    RETURN nextval(sequence_name)::INT;
END;
$$ LANGUAGE plpgsql;