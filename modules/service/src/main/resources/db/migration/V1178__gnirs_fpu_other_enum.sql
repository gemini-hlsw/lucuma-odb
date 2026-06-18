-- Introduce a proper enum for the GNIRS non-slit ("other") FPU values
-- (previously bare tags).
--
-- The enum is also needed by the GNIRS smart gcal table, which now includes
-- the daytime pinhole flats used to trace the cross-dispersed spectral orders.

CREATE TYPE e_gnirs_fpu_other AS ENUM ('Acquisition', 'PupilViewer', 'Pinhole1', 'Pinhole3');

-- 1) Apply the enum to the existing dynamic-config column. v_gnirs_dynamic
--    selects t.* from t_gnirs_dynamic, so it must be dropped and recreated
--    around the column type change.
DROP VIEW v_gnirs_dynamic;

ALTER TABLE t_gnirs_dynamic
  ALTER COLUMN c_fpu_other TYPE e_gnirs_fpu_other USING c_fpu_other::text::e_gnirs_fpu_other;

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

-- 2) Smart gcal: replace the slit-only c_fpu with c_fpu_slit / c_fpu_other.
--    The table contents are reloaded by the repeatable R__SmartGnirs migration
--    (importForcingVersion bumped).

-- The composite search index includes c_fpu, so drop it before dropping the
-- column. R__SmartGnirs recreates it over the new fpu columns.
DROP INDEX IF EXISTS i_smart_gnirs;

-- Existing rows only have c_fpu and would violate the new not-null-exactly-one
-- check; the repeatable loader repopulates the table.
TRUNCATE TABLE t_smart_gnirs;

ALTER TABLE t_smart_gnirs
  DROP COLUMN c_fpu,
  ADD COLUMN c_fpu_slit  d_tag             REFERENCES t_gnirs_fpu_slit(c_tag),
  ADD COLUMN c_fpu_other e_gnirs_fpu_other,
  ADD CONSTRAINT t_smart_gnirs_fpu_check CHECK (
    (c_fpu_slit IS NOT NULL AND c_fpu_other IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_other IS NOT NULL)
  );
