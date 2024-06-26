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
            -- Since it is submitted it must have a semester and subtype.
            -- If there was a type
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
            -- Since it is not submitted and the semester has changed, we lose
            -- any index we may have had while previously submitted
            NEW.c_semester_index := NULL;
          END IF;
          NEW.c_instrument   := NULL;
          NEW.c_library_desc := NULL;
        END;
    END CASE;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;