
-- Programs may have a calibration role
alter table t_program
add column c_calibration_role e_calibration_role null;

ALTER TYPE e_program_type ADD VALUE 'system';
COMMIT;
INSERT INTO t_program_type VALUES ('system', 'SYS', 'System', false);
COMMIT;

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

      WHEN NEW.c_program_type = 'system' THEN
        BEGIN
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
BEGIN
    -- Update calibration role in target based on value from program
  NEW.c_calibration_role = (SELECT c_calibration_role FROM t_program WHERE c_program_id = NEW.c_program_id);

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_target_calibration_target
  AFTER INSERT OR UPDATE ON t_target
  DEFERRABLE
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

-- Spectrophotometric calibration program
insert into t_program (c_program_id, c_name, c_library_desc, c_calibration_role) values ('p-10', 'Spectrophotometric targets', 'SPECTROPHOTOMETRIC', 'spectrophotometric');

INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-100', 'BD+28  4211', 'present', 'sidereal', 1180065332865, 103910367627, 'J2000.000', -34809, -56937, 0.000, 8917, 'simbad', 'BD+28  4211', 'HotSubdwarf, sdO2VIIIHe5', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "8.922"}, {"band": "B", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "10.25"}, {"band": "V", "error": "0.05", "units": "VEGA_MAGNITUDE", "value": "10.58"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.656"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.831"}, {"band": "J", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "11.275"}, {"band": "H", "error": "0.036", "units": "VEGA_MAGNITUDE", "value": "11.438"}, {"band": "K", "error": "0.028", "units": "VEGA_MAGNITUDE", "value": "11.556"}, {"band": "GAIA", "error": "0.002851", "units": "VEGA_MAGNITUDE", "value": "10.45304"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-101', 'CD-32  9927', 'present', 'sidereal', 766594841595, 1177005626732, 'J2000.000', -3147, 6736, -5.2000, 2651, 'simbad', 'CD-32  9927', 'Star, A4', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "SLOAN_G", "error": "0.268", "units": "VEGA_MAGNITUDE", "value": "10.845"}, {"band": "SLOAN_I", "error": "0.11", "units": "VEGA_MAGNITUDE", "value": "10.54"}, {"band": "B", "error": "0.071", "units": "VEGA_MAGNITUDE", "value": "10.793"}, {"band": "V", "error": "0.025", "units": "VEGA_MAGNITUDE", "value": "10.444"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.12"}, {"band": "I", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "10.106"}, {"band": "J", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "9.9"}, {"band": "H", "error": "0.022", "units": "VEGA_MAGNITUDE", "value": "9.791"}, {"band": "K", "error": "0.025", "units": "VEGA_MAGNITUDE", "value": "9.766"}, {"band": "GAIA", "error": "0.002793", "units": "VEGA_MAGNITUDE", "value": "10.392047"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-102', 'CD-34   241', 'present', 'sidereal', 37603803465, 1174851569954, 'J2000.000', -17002, -18818, -8.93000, 3628, 'simbad', 'CD-34   241', 'Star, F', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "SLOAN_G", "error": "0.009", "units": "VEGA_MAGNITUDE", "value": "11.4"}, {"band": "SLOAN_R", "error": "0.017", "units": "VEGA_MAGNITUDE", "value": "11.084"}, {"band": "SLOAN_I", "error": "0.018", "units": "VEGA_MAGNITUDE", "value": "10.998"}, {"band": "B", "error": "0.041", "units": "VEGA_MAGNITUDE", "value": "11.696"}, {"band": "V", "error": "0.016", "units": "VEGA_MAGNITUDE", "value": "11.208"}, {"band": "J", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "10.253"}, {"band": "H", "error": "0.025", "units": "VEGA_MAGNITUDE", "value": "9.996"}, {"band": "K", "error": "0.021", "units": "VEGA_MAGNITUDE", "value": "9.961"}, {"band": "GAIA", "error": "0.002775", "units": "VEGA_MAGNITUDE", "value": "11.099415"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-103', 'LAWD 74', 'present', 'sidereal', 1044523843920, 1268399934043, 'J2000.000', -61224, -161846, 0.000, 95176, 'simbad', 'LAWD 74', 'WhiteDwarf, DBQA5', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.553"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.345"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.29"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.229"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.164"}, {"band": "J", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "12.351"}, {"band": "H", "error": "0.025", "units": "VEGA_MAGNITUDE", "value": "12.355"}, {"band": "K", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "12.421"}, {"band": "GAIA", "error": "0.002789", "units": "VEGA_MAGNITUDE", "value": "12.253493"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-104', 'CPD-69   177', 'present', 'sidereal', 171465293940, 1049036619232, 'J2000.000', 39668, -103237, 62.6000, 96183, 'simbad', 'CPD-69   177', 'WhiteDwarf, DA3.0', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.757"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.413"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.394"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.47"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.558"}, {"band": "J", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "11.758"}, {"band": "H", "error": "0.027", "units": "VEGA_MAGNITUDE", "value": "11.79"}, {"band": "K", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "11.861"}, {"band": "GAIA", "error": "0.002797", "units": "VEGA_MAGNITUDE", "value": "11.409583"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-105', 'CD-38 10980', 'present', 'sidereal', 885207570735, 1154773838957, 'J2000.000', 77398, 386, 44.000, 77455, 'simbad', 'CD-38 10980', 'WhiteDwarf, DA2', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "9.916"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.885"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.029"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.936"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.841"}, {"band": "J", "error": "0.021", "units": "VEGA_MAGNITUDE", "value": "11.577"}, {"band": "H", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "11.708"}, {"band": "K", "error": "0.022", "units": "VEGA_MAGNITUDE", "value": "11.768"}, {"band": "GAIA", "error": "0.002805", "units": "VEGA_MAGNITUDE", "value": "11.0004"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-106', 'Feige 110', 'present', 'sidereal', 1259975995260, 1277403828858, 'J2000.000', -8533, -592, 0.000, 3692, 'simbad', 'Feige 110', 'HotSubdwarf, sdO8VIIIHe5', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.36"}, {"band": "B", "error": "0.09", "units": "VEGA_MAGNITUDE", "value": "11.45"}, {"band": "V", "error": "0.14", "units": "VEGA_MAGNITUDE", "value": "11.5"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.97"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.145"}, {"band": "J", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "12.548"}, {"band": "H", "error": "0.027", "units": "VEGA_MAGNITUDE", "value": "12.663"}, {"band": "K", "error": "0.034", "units": "VEGA_MAGNITUDE", "value": "12.796"}, {"band": "GAIA", "error": "0.002812", "units": "VEGA_MAGNITUDE", "value": "11.772521"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-107', 'Feige  34', 'present', 'sidereal', 575651038245, 155169212615, 'J2000.000', 13874, -25586, 30.3000, 4361, 'simbad', 'Feige  34', 'HotSubdwarf, sdOp', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "9.613"}, {"band": "B", "error": "0.04", "units": "VEGA_MAGNITUDE", "value": "10.91"}, {"band": "V", "error": "0.07", "units": "VEGA_MAGNITUDE", "value": "11.14"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.319"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.464"}, {"band": "J", "error": "0.022", "units": "VEGA_MAGNITUDE", "value": "11.643"}, {"band": "H", "error": "0.021", "units": "VEGA_MAGNITUDE", "value": "11.563"}, {"band": "K", "error": "0.019", "units": "VEGA_MAGNITUDE", "value": "11.54"}, {"band": "GAIA", "error": "0.002805", "units": "VEGA_MAGNITUDE", "value": "11.106956"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-10b', 'BD+52   913', 'present', 'sidereal', 274959271455, 190191919301, 'J2000.000', 12701, -93416, 69.000, 19050, 'simbad', 'BD+52   913', 'WhiteDwarf, DA.8', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.25"}, {"band": "B", "error": "0.08", "units": "VEGA_MAGNITUDE", "value": "11.44"}, {"band": "V", "error": "0.17", "units": "VEGA_MAGNITUDE", "value": "11.69"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.93"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.108"}, {"band": "J", "error": "0.021", "units": "VEGA_MAGNITUDE", "value": "12.543"}, {"band": "H", "error": "0.025", "units": "VEGA_MAGNITUDE", "value": "12.669"}, {"band": "K", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "12.764"}, {"band": "GAIA", "error": "0.002842", "units": "VEGA_MAGNITUDE", "value": "11.718442"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-10f', 'CD-28   595', 'present', 'sidereal', 103354054830, 1197084260861, 'J2000.000', 310967, -232066, 101.14000, 7336, 'simbad', 'CD-28   595', 'HighPM*', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.06"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.51"}, {"band": "J", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "10.304"}, {"band": "H", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "9.968"}, {"band": "K", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "9.903"}, {"band": "GAIA", "error": "0.00277", "units": "VEGA_MAGNITUDE", "value": "11.342707"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-111', 'L  595-22', 'present', 'sidereal', 320771128560, 1195707638239, 'J2000.000', 236540, -157283, 253.4000, 2590, 'simbad', 'L  595-22', 'ChemPec*, sdG', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": "0.13", "units": "VEGA_MAGNITUDE", "value": "12.31"}, {"band": "V", "error": "0.17", "units": "VEGA_MAGNITUDE", "value": "12.38"}, {"band": "J", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "11.224"}, {"band": "H", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "11.009"}, {"band": "K", "error": "0.021", "units": "VEGA_MAGNITUDE", "value": "10.941"}, {"band": "GAIA", "error": "0.002765", "units": "VEGA_MAGNITUDE", "value": "12.0797"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-116', 'WG  22', 'present', 'sidereal', 682946716830, 1116719780485, 'J2000.000', -557111, -74036, 73.000, 67406, 'simbad', 'WG  22', 'WhiteDwarf, DA4.2', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "14.14"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "13.96"}, {"band": "J", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "13.806"}, {"band": "H", "error": "0.036", "units": "VEGA_MAGNITUDE", "value": "13.815"}, {"band": "K", "error": "0.062", "units": "VEGA_MAGNITUDE", "value": "13.907"}, {"band": "GAIA", "error": "0.002802", "units": "VEGA_MAGNITUDE", "value": "13.792915"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-118', 'CD-44 12736', 'present', 'sidereal', 1004789253705, 1136483092408, 'J2000.000', -169788, -159812, -4.97000, 9444, 'simbad', 'CD-44 12736', 'HighPM*, G0', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.83"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.22"}, {"band": "J", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "9.034"}, {"band": "H", "error": "0.055", "units": "VEGA_MAGNITUDE", "value": "8.719"}, {"band": "K", "error": "0.019", "units": "VEGA_MAGNITUDE", "value": "8.644"}, {"band": "GAIA", "error": "0.002762", "units": "VEGA_MAGNITUDE", "value": "10.061889"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-11b', 'EGGR 264', 'present', 'sidereal', 1259630822790, 1234471534427, 'J2000.000', 242074, 10229, 0.000, 39146, 'simbad', 'EGGR 264', 'WhiteDwarf, DB3', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "13.279"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "14.132"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "14.111"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "14.07"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "14.049"}, {"band": "J", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "14.172"}, {"band": "H", "error": "0.045", "units": "VEGA_MAGNITUDE", "value": "14.194"}, {"band": "K", "error": "0.077", "units": "VEGA_MAGNITUDE", "value": "14.255"}, {"band": "GAIA", "error": "0.002831", "units": "VEGA_MAGNITUDE", "value": "14.059093"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-108', 'HD 105183', 'present', 'sidereal', 654108522570, 42012659833, 'J2000.000', -7114, -6589, 29.5000, 455, 'simbad', 'HD 105183', 'HotSubdwarf, sdB8IIIHe2', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.93"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.06"}, {"band": "J", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "11.279"}, {"band": "H", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "11.35"}, {"band": "K", "error": "0.019", "units": "VEGA_MAGNITUDE", "value": "11.378"}, {"band": "GAIA", "error": "0.003008", "units": "VEGA_MAGNITUDE", "value": "11.013108"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-10a', 'EGGR 382', 'present', 'sidereal', 30516944040, 1252321224700, 'J2000.000', 157577, -191576, -361.75000, 2166, 'simbad', 'EGGR 382', 'HighPM*, DC?', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "15.511"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "15.572"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "14.891"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "14.467"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "14.051"}, {"band": "GAIA", "error": "0.002775", "units": "VEGA_MAGNITUDE", "value": "14.680038"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-10d', 'HD 289002', 'present', 'sidereal', 364700588235, 7694685250, 'J2000.000', -1092, -342, 49.663101000, 456, 'simbad', 'HD 289002', 'Star, B1', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.06"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.62"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.44"}, {"band": "J", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "10.074"}, {"band": "H", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "10.073"}, {"band": "K", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "10.059"}, {"band": "GAIA", "error": "0.002806", "units": "VEGA_MAGNITUDE", "value": "10.415216"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-110', 'LP  995-86', 'present', 'sidereal', 205539188505, 1155082986320, 'J2000.000', 214130, -189436, 25.71000, 2839, 'simbad', 'LP  995-86', 'ChemPec*, F', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "13.61"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "13.15"}, {"band": "J", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "12.089"}, {"band": "H", "error": "0.025", "units": "VEGA_MAGNITUDE", "value": "11.792"}, {"band": "K", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "11.745"}, {"band": "GAIA", "error": "0.002768", "units": "VEGA_MAGNITUDE", "value": "13.000236"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-113', 'CD-32  5613', 'present', 'sidereal', 469386436275, 1177407084177, 'J2000.000', -1061158, 1345900, 53.000, 117396, 'simbad', 'CD-32  5613', 'WhiteDwarf, DA5.5', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.07"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.85"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.3"}, {"band": "J", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "11.578"}, {"band": "H", "error": "0.033", "units": "VEGA_MAGNITUDE", "value": "11.539"}, {"band": "K", "error": "0.029", "units": "VEGA_MAGNITUDE", "value": "11.547"}, {"band": "GAIA", "error": "0.002769", "units": "VEGA_MAGNITUDE", "value": "11.822395"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-115', 'LAWD 37', 'present', 'sidereal', 635143754010, 1062570538043, 'J2000.000', 2661640, -344933, 0.000, 215675, 'simbad', 'LAWD 37', 'WhiteDwarf, DQ', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "SLOAN_G", "error": "0", "units": "VEGA_MAGNITUDE", "value": "11.54"}, {"band": "SLOAN_R", "error": "0.06", "units": "VEGA_MAGNITUDE", "value": "11.53"}, {"band": "SLOAN_I", "error": "0.07", "units": "VEGA_MAGNITUDE", "value": "11.57"}, {"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.08"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.725"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.513"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.34"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.163"}, {"band": "J", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "11.188"}, {"band": "H", "error": "0.025", "units": "VEGA_MAGNITUDE", "value": "11.13"}, {"band": "K", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "11.104"}, {"band": "GAIA", "error": "0.00279", "units": "VEGA_MAGNITUDE", "value": "11.423104"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-117', 'CD-28 11464', 'present', 'sidereal', 845094719295, 1193063026366, 'J2000.000', -233579, -177141, -126.07000, 4485, 'simbad', 'CD-28 11464', 'ChemPec*, A', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "SLOAN_R", "error": "0.01", "units": "VEGA_MAGNITUDE", "value": "11.66"}, {"band": "SLOAN_I", "error": "0.02", "units": "VEGA_MAGNITUDE", "value": "11.506"}, {"band": "B", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "12.31"}, {"band": "V", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "11.797"}, {"band": "R", "error": "0.04", "units": "VEGA_MAGNITUDE", "value": "11.794"}, {"band": "J", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "10.63"}, {"band": "H", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "10.382"}, {"band": "K", "error": "0.021", "units": "VEGA_MAGNITUDE", "value": "10.282"}, {"band": "GAIA", "error": "0.002772", "units": "VEGA_MAGNITUDE", "value": "11.632585"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-11a', 'LP  877-23', 'present', 'sidereal', 1235415530970, 1221867005568, 'J2000.000', 78810, -314311, -226.8000, 6299, 'simbad', 'LP  877-23', 'HighPM*', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": "0.39", "units": "VEGA_MAGNITUDE", "value": "13.02"}, {"band": "V", "error": "0.2", "units": "VEGA_MAGNITUDE", "value": "11.9"}, {"band": "J", "error": "0.029", "units": "VEGA_MAGNITUDE", "value": "10.775"}, {"band": "H", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "10.402"}, {"band": "K", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "10.343"}, {"band": "GAIA", "error": "0.002769", "units": "VEGA_MAGNITUDE", "value": "11.887551"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-109', 'BD+25  2534', 'present', 'sidereal', 681652742505, 90239867922, 'J2000.000', 1504, -27222, 1.2000, 7040, 'simbad', 'BD+25  2534', 'HotSubdwarf, sdB1(k)', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "9.117"}, {"band": "B", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "10.26"}, {"band": "V", "error": "0.05", "units": "VEGA_MAGNITUDE", "value": "10.59"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.642"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.809"}, {"band": "J", "error": "0.022", "units": "VEGA_MAGNITUDE", "value": "11.157"}, {"band": "H", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "11.27"}, {"band": "K", "error": "0.022", "units": "VEGA_MAGNITUDE", "value": "11.367"}, {"band": "GAIA", "error": "0.002811", "units": "VEGA_MAGNITUDE", "value": "10.46187"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-10c', 'GD 108', 'present', 'sidereal', 540708693555, 1268789226006, 'J2000.000', -42395, -2079, 90.5000, 1897, 'simbad', 'GD 108', 'HotSubdwarf, sdB', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": "0.0026", "units": "VEGA_MAGNITUDE", "value": "12.406"}, {"band": "B", "error": "0.0029", "units": "VEGA_MAGNITUDE", "value": "13.349"}, {"band": "V", "error": "0.0025", "units": "VEGA_MAGNITUDE", "value": "13.563"}, {"band": "R", "error": "0.0025", "units": "VEGA_MAGNITUDE", "value": "13.662"}, {"band": "I", "error": "0.0007", "units": "VEGA_MAGNITUDE", "value": "13.781"}, {"band": "J", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "14.098"}, {"band": "H", "error": "0.039", "units": "VEGA_MAGNITUDE", "value": "14.139"}, {"band": "K", "error": "0.088", "units": "VEGA_MAGNITUDE", "value": "14.256"}, {"band": "GAIA", "error": "0.002853", "units": "VEGA_MAGNITUDE", "value": "13.530314"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-10e', 'HZ 44', 'present', 'sidereal', 723228944160, 130079544140, 'J2000.000', -66004, -4408, -26.000, 2670, 'simbad', 'HZ 44', 'HotSubdwarf, sdBN0VIIHe28', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "SLOAN_G", "error": "0.02", "units": "VEGA_MAGNITUDE", "value": "11.45"}, {"band": "SLOAN_R", "error": "0.04", "units": "VEGA_MAGNITUDE", "value": "11.89"}, {"band": "SLOAN_I", "error": "0.05", "units": "VEGA_MAGNITUDE", "value": "12.23"}, {"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.186"}, {"band": "B", "error": "0.04", "units": "VEGA_MAGNITUDE", "value": "11.42"}, {"band": "V", "error": "0.05", "units": "VEGA_MAGNITUDE", "value": "11.65"}, {"band": "R", "error": "0.08", "units": "VEGA_MAGNITUDE", "value": "12"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.995"}, {"band": "J", "error": "0.022", "units": "VEGA_MAGNITUDE", "value": "12.386"}, {"band": "H", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "12.569"}, {"band": "K", "error": "0.027", "units": "VEGA_MAGNITUDE", "value": "12.672"}, {"band": "GAIA", "error": "0.002815", "units": "VEGA_MAGNITUDE", "value": "11.613195"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-112', 'LAWD 25', 'present', 'sidereal', 414311830005, 1233310843133, 'J2000.000', 1138690, -542556, -10.000, 109344, 'simbad', 'LAWD 25', 'WhiteDwarf, DZQA6', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": "0.03", "units": "VEGA_MAGNITUDE", "value": "13.36"}, {"band": "V", "error": "0.002", "units": "VEGA_MAGNITUDE", "value": "13.061"}, {"band": "R", "error": "0.002", "units": "VEGA_MAGNITUDE", "value": "12.886"}, {"band": "I", "error": "0.02", "units": "VEGA_MAGNITUDE", "value": "12.7"}, {"band": "J", "error": "0.022", "units": "VEGA_MAGNITUDE", "value": "12.653"}, {"band": "H", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "12.611"}, {"band": "K", "error": "0.036", "units": "VEGA_MAGNITUDE", "value": "12.583"}, {"band": "GAIA", "error": "0.002768", "units": "VEGA_MAGNITUDE", "value": "12.970207"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-114', 'CD-34  6792', 'present', 'sidereal', 569004278310, 1167738288224, 'J2000.000', -260781, -4479, 186.2000, 3344, 'simbad', 'CD-34  6792', 'HighPM*, F:wl:', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "B", "error": "0.26", "units": "VEGA_MAGNITUDE", "value": "12.84"}, {"band": "V", "error": "0.14", "units": "VEGA_MAGNITUDE", "value": "11.84"}, {"band": "J", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "11.03"}, {"band": "H", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "10.783"}, {"band": "K", "error": "0.023", "units": "VEGA_MAGNITUDE", "value": "10.727"}, {"band": "GAIA", "error": "0.002762", "units": "VEGA_MAGNITUDE", "value": "12.015685"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-119', 'CD-30 17706', 'present', 'sidereal', 1089852730365, 1187213372858, 'J2000.000', -340866, -249423, 73.6000, 61694, 'simbad', 'CD-30 17706', 'WhiteDwarf, DA3.1', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.654"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.286"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.242"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.311"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "12.381"}, {"band": "J", "error": "0.024", "units": "VEGA_MAGNITUDE", "value": "12.583"}, {"band": "H", "error": "0.027", "units": "VEGA_MAGNITUDE", "value": "12.641"}, {"band": "K", "error": "0.032", "units": "VEGA_MAGNITUDE", "value": "12.698"}, {"band": "GAIA", "error": "0.002787", "units": "VEGA_MAGNITUDE", "value": "12.258787"}]}}, "uniform": null, "gaussian": null}', NULL);
INSERT INTO public.t_target (c_program_id, c_target_id, c_name, c_existence, c_type, c_sid_ra, c_sid_dec, c_sid_epoch, c_sid_pm_ra, c_sid_pm_dec, c_sid_rv, c_sid_parallax, c_sid_catalog_name, c_sid_catalog_id, c_sid_catalog_object_type, c_nsid_des, c_nsid_key_type, c_nsid_key, c_source_profile, c_calibration_role) VALUES ('p-10', 't-11c', 'HD 340611', 'present', 'sidereal', 1110928270515, 90229750446, 'J2000.000', -403387, -563404, 71.000, 67409, 'simbad', 'HD 340611', 'WhiteDwarf, DA2.4', NULL, NULL, NULL, '{"point": {"emissionLines": null, "bandNormalized": {"sed": null, "brightnesses": [{"band": "U", "error": null, "units": "VEGA_MAGNITUDE", "value": "10.687"}, {"band": "B", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.471"}, {"band": "V", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.546"}, {"band": "R", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.648"}, {"band": "I", "error": null, "units": "VEGA_MAGNITUDE", "value": "11.728"}, {"band": "J", "error": "0.026", "units": "VEGA_MAGNITUDE", "value": "12.039"}, {"band": "H", "error": "0.032", "units": "VEGA_MAGNITUDE", "value": "12.072"}, {"band": "K", "error": "0.027", "units": "VEGA_MAGNITUDE", "value": "12.186"}, {"band": "GAIA", "error": "0.002801", "units": "VEGA_MAGNITUDE", "value": "11.536574"}]}}, "uniform": null, "gaussian": null}', NULL);

