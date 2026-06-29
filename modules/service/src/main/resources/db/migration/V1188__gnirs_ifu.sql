--- GNIRS IFU FPU
--- The low- and high-resolution integral field units. These are spectroscopy
--- FPUs distinct from the long-slit (t_gnirs_fpu_slit) and non-slit "other"
--- (e_gnirs_fpu_other) apertures. Modeled as an enum (rather than a lookup
--- table) because the slit width is not needed here for sequence generation.
CREATE TYPE e_gnirs_fpu_ifu AS ENUM ('LowResolution', 'HighResolution');

--- GNIRS IFU deckers, matching the new GnirsDecker cases in lucuma-core. The
--- values are added now to keep e_gnirs_decker in sync with the enum; IFU
--- sequence generation (which selects them) comes in a later change.
ALTER TYPE e_gnirs_decker ADD VALUE 'LowResolutionIfu';
ALTER TYPE e_gnirs_decker ADD VALUE 'HighResolutionIfu';

--- Spectroscopy config options: allow an IFU FPU in place of a long-slit FPU.
--- Rename c_fpu to c_fpu_slit (matching t_gnirs_dynamic / t_smart_gnirs) and
--- require exactly one of c_fpu_slit / c_fpu_ifu.
ALTER TABLE t_spectroscopy_config_option_gnirs RENAME COLUMN c_fpu TO c_fpu_slit;

ALTER TABLE t_spectroscopy_config_option_gnirs
  ALTER COLUMN c_fpu_slit DROP NOT NULL,
  ADD COLUMN c_fpu_ifu e_gnirs_fpu_ifu,
  ADD CONSTRAINT t_spectroscopy_config_option_gnirs_fpu_check CHECK (
    (c_fpu_slit IS NOT NULL AND c_fpu_ifu IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_ifu IS NOT NULL)
  );

--- Dynamic config: add the IFU FPU as a third alternative to slit / other.
--- v_gnirs_dynamic is "SELECT t.*", so it must be dropped and recreated to pick
--- up the new column.
DROP VIEW v_gnirs_dynamic;

ALTER TABLE t_gnirs_dynamic
  ADD COLUMN c_fpu_ifu e_gnirs_fpu_ifu,
  DROP CONSTRAINT gnirs_fpu_check,
  ADD CONSTRAINT gnirs_fpu_check CHECK (
    (c_fpu_slit IS NOT NULL AND c_fpu_other IS     NULL AND c_fpu_ifu IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_other IS NOT NULL AND c_fpu_ifu IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_other IS     NULL AND c_fpu_ifu IS NOT NULL)
  );

CREATE VIEW v_gnirs_dynamic AS
  SELECT t.*,
  CASE WHEN t.c_prism IS NOT NULL THEN t.c_step_id END AS c_acquisition_mirror_out_id,
  COALESCE(
    t.c_grating_wavelength,
    (SELECT f.c_wavelength FROM t_gnirs_filter f WHERE f.c_tag = t.c_filter),
    1650000::d_wavelength_pm
  ) AS c_central_wavelength
FROM
  t_gnirs_dynamic t;
