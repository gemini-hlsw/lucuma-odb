-- GNIRS cross-dispersed observations require a daytime pinhole flat
-- calibration (used to trace the cross-dispersed spectral orders). Add the
-- corresponding calibration role.
ALTER TYPE e_calibration_role ADD VALUE 'daytime_pinhole';
