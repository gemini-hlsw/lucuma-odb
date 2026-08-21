-- A MOS mask is a physically machined plate, cut for one instrument.  A
-- GMOS-North mask cannot be mounted in GMOS-South and neither fits
-- Flamingos-2, so a MOS observation's mask attachment must be one cut for that
-- observation's instrument.
--
-- The instrument gets its own column because a foreign key cannot reach inside a
-- JSON document, and the foreign key is the whole mechanism.  It is generated
-- rather than written

ALTER TABLE t_attachment
  ADD COLUMN c_mask_instrument d_tag
    GENERATED ALWAYS AS (
      -- In here we have a missmatch between the gpp instrument names and the ones stored
      -- as Json in the mask column blob.
      CASE c_mask_definition->>'instrument'
        WHEN 'GMOS_NORTH' THEN 'GmosNorth'
        WHEN 'GMOS_SOUTH' THEN 'GmosSouth'
        WHEN 'FLAMINGOS2' THEN 'Flamingos2'
      END
    ) STORED
    REFERENCES t_instrument(c_tag);

COMMENT ON COLUMN t_attachment.c_mask_instrument IS
  'Instrument the MOS mask plate was cut for, read from the mask file at upload.  Exists to be the target of each MOS mode''s mask foreign key.';

-- MOS masks uploaded before V1271 have no definition, so no instrument can be
-- read for them.
-- This is a bit heavy handed but given we only have a couple of masks it is ok

DELETE FROM t_attachment
  WHERE c_attachment_type = 'mos_mask'
    AND c_mask_definition IS NULL;

-- Reset any possible assignment to non matching masks.
UPDATE t_gmos_north_mos m
  SET c_mask_attachment_id   = NULL,
      c_mask_attachment_type = NULL
  WHERE m.c_mask_attachment_id IS NOT NULL
    AND NOT EXISTS (
      SELECT 1 FROM t_attachment a
        WHERE a.c_attachment_id   = m.c_mask_attachment_id
          AND a.c_mask_instrument = m.c_instrument
    );

UPDATE t_gmos_south_mos m
  SET c_mask_attachment_id   = NULL,
      c_mask_attachment_type = NULL
  WHERE m.c_mask_attachment_id IS NOT NULL
    AND NOT EXISTS (
      SELECT 1 FROM t_attachment a
        WHERE a.c_attachment_id   = m.c_mask_attachment_id
          AND a.c_mask_instrument = m.c_instrument
    );

UPDATE t_flamingos_2_mos m
  SET c_mask_attachment_id   = NULL,
      c_mask_attachment_type = NULL
  WHERE m.c_mask_attachment_id IS NOT NULL
    AND NOT EXISTS (
      SELECT 1 FROM t_attachment a
        WHERE a.c_attachment_id   = m.c_mask_attachment_id
          AND a.c_mask_instrument = m.c_instrument
    );

-- The instrument is present exactly for MOS masks, mirroring c_mask_name.

ALTER TABLE t_attachment
  ADD CONSTRAINT t_attachment_mask_instrument_check
    CHECK ((c_attachment_type = 'mos_mask') = (c_mask_instrument IS NOT NULL));

-- This asserts nothing: c_attachment_id is already the primary key, so any
-- superset of it is trivially unique.  It exists because a foreign key's
-- referenced columns must be covered by a unique constraint on exactly those
-- columns, and the mask keys below reference four.
--
-- It does not replace t_attachment_unique, which covers the same first three
-- columns and is still the target of t_obs_attachment_assignment's foreign key.
-- The two look redundant and are not: a 4-column constraint cannot serve a
-- 3-column key.

ALTER TABLE t_attachment
  ADD CONSTRAINT t_attachment_mask_instrument_unique
    UNIQUE (c_program_id, c_attachment_id, c_attachment_type, c_mask_instrument);

-- Widen each MOS mode's mask foreign key by the instrument.  This is required
-- to guard against the case a mask is re-uploaded for a different instrument.
ALTER TABLE t_gmos_north_mos
  DROP CONSTRAINT gmos_north_mos_mask_attachment_fkey,
  ADD  CONSTRAINT gmos_north_mos_mask_attachment_fkey
    FOREIGN KEY (c_program_id, c_mask_attachment_id, c_mask_attachment_type, c_instrument)
    REFERENCES t_attachment (c_program_id, c_attachment_id, c_attachment_type, c_mask_instrument)
    ON DELETE SET NULL (c_mask_attachment_id, c_mask_attachment_type);

ALTER TABLE t_gmos_south_mos
  DROP CONSTRAINT gmos_south_mos_mask_attachment_fkey,
  ADD  CONSTRAINT gmos_south_mos_mask_attachment_fkey
    FOREIGN KEY (c_program_id, c_mask_attachment_id, c_mask_attachment_type, c_instrument)
    REFERENCES t_attachment (c_program_id, c_attachment_id, c_attachment_type, c_mask_instrument)
    ON DELETE SET NULL (c_mask_attachment_id, c_mask_attachment_type);

ALTER TABLE t_flamingos_2_mos
  DROP CONSTRAINT flamingos_2_mos_mask_attachment_fkey,
  ADD  CONSTRAINT flamingos_2_mos_mask_attachment_fkey
    FOREIGN KEY (c_program_id, c_mask_attachment_id, c_mask_attachment_type, c_instrument)
    REFERENCES t_attachment (c_program_id, c_attachment_id, c_attachment_type, c_mask_instrument)
    ON DELETE SET NULL (c_mask_attachment_id, c_mask_attachment_type);
