-- An IGRINS-2 telluric's explicit slit offsets used to be written by
-- Igrins2LongSlitService.applyIgrins2TelluricDefaults with the same
-- NodAlongSlit pattern v_igrins_2_long_slit already computes as the default.
-- Left as an explicit override, the API reported a telluric as customizing a
-- science default it never actually chose, and reverting it restored the
-- science pattern. Now that resetTelluricConfig clears the columns instead of
-- writing them, null out existing tellurics whose explicit offsets just
-- duplicate the default so they fall back to it like new ones do. A telluric
-- whose explicit value differs from the pattern was edited by hand and is
-- left alone.
UPDATE t_igrins_2_long_slit m
SET c_slit_offset_mode = NULL, c_telescope_configs = NULL
FROM v_igrins_2_long_slit v
WHERE v.c_observation_id = m.c_observation_id
  AND is_telluric_calibration(m.c_observation_id)
  AND m.c_slit_offset_mode = v.c_slit_offset_mode_default
  AND m.c_telescope_configs::jsonb = v.c_telescope_configs_default::jsonb;
