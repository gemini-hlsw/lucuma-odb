-- CalibrationSetType gained SmartFlat, SmartArc and DayBaseline in lucuma-core
-- 0.273.0.  Appended in the Scala declaration order.

ALTER TYPE e_calibration_set_type ADD VALUE 'smart_flat';
ALTER TYPE e_calibration_set_type ADD VALUE 'smart_arc';
ALTER TYPE e_calibration_set_type ADD VALUE 'day_baseline';
