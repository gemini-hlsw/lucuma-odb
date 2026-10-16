-- Daytime calibrations: staff-created calibration observations with no target,
-- ITC, guide star or acquisition, whose steps are all day calibrations.  The
-- observing mode names the instrument and the calibration role is the flag
-- every calibration rule keys on.  New enum values cannot be used in the same
-- transaction that adds them, so the tables and triggers follow in V1351.
ALTER TYPE e_calibration_role    ADD VALUE 'daytime_calibration';
ALTER TYPE e_observing_mode_type ADD VALUE 'daytime_calibration';
