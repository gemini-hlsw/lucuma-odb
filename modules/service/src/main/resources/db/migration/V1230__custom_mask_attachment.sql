-- A custom FPU mask now references a MOS mask attachment instead of carrying a
-- free-text filename, and the reference may be absent because for MOS the mask
-- is often only defined during Phase 2.
--
-- Because a mask that is yet to be defined has a null attachment id, the slit
-- width becomes the sole discriminator for "this row has a custom mask".

-- V0934 renamed this domain to c_attachment_id, which reads like a column name
-- and is the only domain not following the d_* convention. Restore it before
-- using it as a column type below.

ALTER DOMAIN c_attachment_id RENAME TO d_attachment_id;

DROP VIEW v_gmos_north_dynamic;
DROP VIEW v_gmos_south_dynamic;
DROP VIEW v_flamingos_2_dynamic;

-- GMOS North

ALTER TABLE t_gmos_north_dynamic
  DROP CONSTRAINT gmos_north_fpu_check,
  DROP COLUMN     c_fpu_custom_mask_filename,
  ADD COLUMN      c_fpu_custom_mask_attachment_id d_attachment_id NULL
                    REFERENCES t_attachment(c_attachment_id),
  ADD CONSTRAINT  gmos_north_fpu_check CHECK (
    (
      c_fpu_custom_mask_slit_width    IS NULL AND
      c_fpu_custom_mask_attachment_id IS NULL
    ) OR (
      c_fpu_custom_mask_slit_width    IS NOT NULL AND
      c_fpu_builtin                   IS NULL
    )
  );

-- GMOS South

ALTER TABLE t_gmos_south_dynamic
  DROP CONSTRAINT gmos_south_fpu_check,
  DROP COLUMN     c_fpu_custom_mask_filename,
  ADD COLUMN      c_fpu_custom_mask_attachment_id d_attachment_id NULL
                    REFERENCES t_attachment(c_attachment_id),
  ADD CONSTRAINT  gmos_south_fpu_check CHECK (
    (
      c_fpu_custom_mask_slit_width    IS NULL AND
      c_fpu_custom_mask_attachment_id IS NULL
    ) OR (
      c_fpu_custom_mask_slit_width    IS NOT NULL AND
      c_fpu_builtin                   IS NULL
    )
  );

-- Flamingos 2

ALTER TABLE t_flamingos_2_dynamic
  DROP CONSTRAINT f2_fpu_check,
  DROP COLUMN     c_fpu_custom_mask_filename,
  ADD COLUMN      c_fpu_custom_mask_attachment_id d_attachment_id NULL
                    REFERENCES t_attachment(c_attachment_id),
  ADD CONSTRAINT  f2_fpu_check CHECK (
    (
      c_fpu_custom_mask_slit_width    IS NULL AND
      c_fpu_custom_mask_attachment_id IS NULL
    ) OR (
      c_fpu_custom_mask_slit_width    IS NOT NULL AND
      c_fpu_builtin                   IS NULL
    )
  );

-- Recreate the views, discriminating on the slit width rather than the filename.

CREATE VIEW v_gmos_north_dynamic AS
  SELECT t.*,
  CASE WHEN t.c_grating_disperser          IS NOT NULL                                THEN t.c_step_id END AS c_grating_id,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL                                THEN t.c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_step_id END AS c_fpu_id,
  CASE WHEN t.c_grating_wavelength         IS NOT NULL OR t.c_filter      IS NOT NULL THEN t.c_step_id END AS c_central_wavelength_id,
  COALESCE (
    t.c_grating_wavelength,
    (SELECT f.c_wavelength FROM t_gmos_north_filter f WHERE f.c_tag = t.c_filter)
  ) AS c_central_wavelength
FROM
  t_gmos_north_dynamic t;

CREATE VIEW v_gmos_south_dynamic AS
SELECT t.*,
  CASE WHEN t.c_grating_disperser          IS NOT NULL                                THEN t.c_step_id END AS c_grating_id,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL                                THEN t.c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_step_id END AS c_fpu_id,
  CASE WHEN t.c_grating_wavelength         IS NOT NULL OR t.c_filter      IS NOT NULL THEN t.c_step_id END AS c_central_wavelength_id,
  COALESCE (
    t.c_grating_wavelength,
    (SELECT f.c_wavelength FROM t_gmos_south_filter f WHERE f.c_tag = t.c_filter)
  ) AS c_central_wavelength
FROM
  t_gmos_south_dynamic t;

CREATE VIEW v_flamingos_2_dynamic AS
  SELECT t.*,
  f.c_wavelength AS c_central_wavelength,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL                                THEN t.c_step_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_step_id END AS c_fpu_id
FROM
  t_flamingos_2_dynamic t
INNER JOIN t_f2_filter f ON t.c_filter = f.c_tag;
