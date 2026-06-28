-- Smart GCAL: allow GNIRS IFU FPUs. The flat/arc calibrations now include LR-IFU /
-- HR-IFU rows, so t_smart_gnirs needs the IFU FPU column alongside slit / other.
-- The table is repopulated by the repeatable R__SmartGnirs migration (importForcingVersion
-- bumped), which also drops/recreates the i_smart_gnirs index over the new column.

ALTER TABLE t_smart_gnirs
  ADD COLUMN c_fpu_ifu e_gnirs_fpu_ifu,
  DROP CONSTRAINT t_smart_gnirs_fpu_check,
  ADD CONSTRAINT t_smart_gnirs_fpu_check CHECK (
    (c_fpu_slit IS NOT NULL AND c_fpu_other IS     NULL AND c_fpu_ifu IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_other IS NOT NULL AND c_fpu_ifu IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_other IS     NULL AND c_fpu_ifu IS NOT NULL)
  );
