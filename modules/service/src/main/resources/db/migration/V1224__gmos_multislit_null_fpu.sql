-- GMOS MOS spectroscopy options are loaded as separate rows whose focal-plane unit
-- is a custom mask rather than a builtin aperture.
-- The builtin FPU column therefore becomes nullable:

ALTER TABLE t_spectroscopy_config_option_gmos_north ALTER COLUMN c_fpu DROP NOT NULL;
ALTER TABLE t_spectroscopy_config_option_gmos_south ALTER COLUMN c_fpu DROP NOT NULL;
