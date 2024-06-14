
-- Programs may have a calibration role
alter table t_program
add column c_calibration_role e_calibration_role null;

ALTER TYPE e_program_type ADD VALUE 'calibration-library';
COMMIT;
INSERT INTO t_program_type VALUES ('calibration-library', 'CLI', 'Calibration Library', false);
COMMIT;

-- Create a trigger function
-- If a program has a calibration role, set the program type to 'CLI'
CREATE OR REPLACE FUNCTION calibration_type_program_type()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.c_calibration_role IS NOT NULL THEN
        -- Set program_type to the fixed value
        NEW.c_program_type = 'calibration-library';
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create a trigger on your_table
CREATE TRIGGER calibration_role_sets_type
BEFORE INSERT OR UPDATE ON t_program
FOR EACH ROW
EXECUTE FUNCTION calibration_type_program_type();

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

          WHEN c_program_type = 'calibration-library' THEN
            c_instrument      IS NULL     AND
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
COMMIT;

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

      WHEN NEW.c_program_type = 'calibration-library' THEN
        BEGIN
          NEW.c_library_desc    := NULL;
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

insert into t_program (c_program_id, c_name, c_calibration_role) values ('p-10', 'G-LIB-STANDARDS-SPECTROPHOTOMETRIC', 'spectrophotometric');
--
-- -- Drop constraints/trigger that are no longer needed because c_role is dropped
-- alter table t_asterism_target
-- drop constraint asterism_target_is_science;
--
-- drop trigger t_target_role_immutable_trigger on t_target;
--
-- -- Need to recreate target view because of added and dropped field in target table.
-- drop view if exists v_target;
--
-- -- a view that has synthetic nullable ids for nullable embedded objects (required by grackle)
-- -- Same as in V0070
-- create view v_target as
--   select *,
--   case when c_type='sidereal'              then c_target_id end as c_sidereal_id,
--   case when c_type='nonsidereal'           then c_target_id end as c_nonsidereal_id,
--   case when c_sid_catalog_name is not null then c_target_id end as c_sid_catalog_info_id,
--   case when c_sid_pm_ra        is not null then c_target_id end as c_sid_pm_id,
--   case when c_sid_parallax     is not null then c_target_id end as c_sid_parallax_id,
--   case when c_sid_rv           is not null then c_target_id end as c_sid_rv_id
--   from t_target;
