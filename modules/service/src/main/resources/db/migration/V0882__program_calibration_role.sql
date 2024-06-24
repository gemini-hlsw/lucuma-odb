
-- Programs may have a calibration role
alter table t_program
add column c_calibration_role e_calibration_role null;

INSERT INTO t_program_type VALUES ('system', 'SYS', 'System', false);

-- Create a trigger function
-- If a program has a calibration role, set the program type to 'SYS'
CREATE OR REPLACE FUNCTION calibration_type_program_type()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.c_calibration_role IS NOT NULL THEN
        -- Set program_type to the fixed value
        NEW.c_program_type = 'system';
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create a trigger on your_table
CREATE TRIGGER calibration_role_sets_type
BEFORE INSERT OR UPDATE ON t_program
FOR EACH ROW
EXECUTE FUNCTION calibration_type_program_type();

-- Update constraints and triggers for the new program type 'calibration-library'
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
            c_instrument   IS NULL     AND
            c_library_desc IS NULL     AND
            (c_proposal_status = 'not_submitted' OR (
              c_semester        IS NOT NULL AND
              c_semester_index  IS NOT NULL AND
              c_science_subtype IS NOT NULL
            ))
      END
  );

-- Update the update_program_type function to handle SYS
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

      WHEN NEW.c_program_type = 'system' THEN
        BEGIN
          IF NEW.c_library_desc IS NULL THEN
            RAISE EXCEPTION 'System programs must define a description';
          END IF;
          NEW.c_semester        := NULL;
          NEW.c_semester_index  := NULL;
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

-- Calibration programs' targets are marked with the same calibration type
CREATE OR REPLACE FUNCTION calibration_targets_on_calibration_programs()
  RETURNS trigger AS $$
DECLARE
    calibration_role e_calibration_role;
BEGIN
    -- Fetch the value from the source table
    SELECT c_calibration_role INTO calibration_role
    FROM t_program
    WHERE c_program_id = NEW.c_program_id;

    NEW.c_calibration_role := calibration_role;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_target_calibration_target
  BEFORE INSERT OR UPDATE ON t_target
  FOR EACH ROW
  EXECUTE PROCEDURE calibration_targets_on_calibration_programs();

-- Update the entry point for formatting to handle SYS
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

      WHEN ptype = 'system' THEN
          CONCAT('SYS-', description)

      WHEN ptype = 'science' AND proposal_status = 'accepted' THEN
          format_science_reference(semester, index, science_subtype)

      ELSE
          NULL
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Forbid changing calibration role
CREATE OR REPLACE FUNCTION forbid_calibration_role_change()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_calibration_role IS DISTINCT FROM OLD.c_calibration_role THEN
    RAISE EXCEPTION 'Modification of calibration role is not allowed';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER forbid_calibration_role_change_trigger
BEFORE UPDATE ON t_program
FOR EACH ROW
EXECUTE FUNCTION forbid_calibration_role_change();
