-- A MOS mask is a physically machined plate, cut for one instrument.  A
-- GMOS-North plate cannot be mounted in GMOS-South and neither fits
-- Flamingos-2, so a MOS observation's mask attachment must be one cut for that
-- observation's instrument.
--
-- Every check applied to the attachment until now -- that it exists, is of type
-- 'mos_mask' and belongs to the same program -- is one a wrong-instrument mask
-- passes cleanly, so the mismatch surfaced only at the telescope.
--
-- The instrument is recorded in its own column rather than read out of
-- c_mask_definition, for two reasons: a foreign key cannot reach inside a JSON
-- document, and masks predating V1271 have no definition to read.

ALTER TABLE t_attachment
  ADD COLUMN c_mask_instrument d_tag NULL REFERENCES t_instrument(c_tag);

COMMENT ON COLUMN t_attachment.c_mask_instrument IS
  'Instrument the MOS mask plate was cut for.  Read from the mask file at upload; for masks predating V1271 inferred from an observation using them.';

-- 1.  Masks with a parsed definition take the instrument from it.  The
-- definition is query-format JSON, so it spells the instrument with its GraphQL
-- name while the column uses the database tag.

UPDATE t_attachment
  SET c_mask_instrument = CASE c_mask_definition->>'instrument'
                            WHEN 'GMOS_NORTH' THEN 'GmosNorth'
                            WHEN 'GMOS_SOUTH' THEN 'GmosSouth'
                            WHEN 'FLAMINGOS2' THEN 'Flamingos2'
                          END
  WHERE c_attachment_type = 'mos_mask'
    AND c_mask_definition IS NOT NULL;

DO $$
DECLARE unmapped text;
BEGIN
  SELECT string_agg(c_mask_definition->>'instrument', ', ')
    INTO unmapped
    FROM t_attachment
    WHERE c_attachment_type = 'mos_mask'
      AND c_mask_definition IS NOT NULL
      AND c_mask_instrument IS NULL;
  IF unmapped IS NOT NULL THEN
    RAISE EXCEPTION 'MOS mask definitions name unrecognised instruments: %', unmapped;
  END IF;
END $$;

-- 2.  Legacy masks -- those with no definition -- take the instrument from any
-- observation or executed step using them.  Both are genuine "this mask is the
-- FPU" claims and are equally authoritative.

CREATE TEMP TABLE tmp_mask_claim AS
  SELECT DISTINCT x.aid, x.instrument
  FROM (
    SELECT c_mask_attachment_id AS aid, c_instrument AS instrument FROM t_gmos_north_mos  WHERE c_mask_attachment_id IS NOT NULL
    UNION ALL
    SELECT c_mask_attachment_id,        c_instrument              FROM t_gmos_south_mos   WHERE c_mask_attachment_id IS NOT NULL
    UNION ALL
    SELECT c_mask_attachment_id,        c_instrument              FROM t_flamingos_2_mos  WHERE c_mask_attachment_id IS NOT NULL
    UNION ALL
    SELECT c_fpu_custom_mask_attachment_id, 'GmosNorth'::d_tag    FROM t_gmos_north_dynamic  WHERE c_fpu_custom_mask_attachment_id IS NOT NULL
    UNION ALL
    SELECT c_fpu_custom_mask_attachment_id, 'GmosSouth'::d_tag    FROM t_gmos_south_dynamic  WHERE c_fpu_custom_mask_attachment_id IS NOT NULL
    UNION ALL
    SELECT c_fpu_custom_mask_attachment_id, 'Flamingos2'::d_tag   FROM t_flamingos_2_dynamic WHERE c_fpu_custom_mask_attachment_id IS NOT NULL
  ) x
  JOIN t_attachment a ON a.c_attachment_id = x.aid
  WHERE a.c_attachment_type = 'mos_mask'
    AND a.c_mask_instrument IS NULL;

-- A legacy mask claimed by two instruments cannot be resolved by inference.  It
-- takes a hand edit or a bug to produce, so the deploy stops rather than
-- guessing and burying the corruption.

DO $$
DECLARE conflicted text;
BEGIN
  SELECT string_agg(aid::text, ', ')
    INTO conflicted
    FROM (
      SELECT aid FROM tmp_mask_claim GROUP BY aid HAVING count(DISTINCT instrument) > 1
    ) c;
  IF conflicted IS NOT NULL THEN
    RAISE EXCEPTION 'MOS mask attachments claimed by more than one instrument: %', conflicted;
  END IF;
END $$;

UPDATE t_attachment a
  SET c_mask_instrument = c.instrument
  FROM tmp_mask_claim c
  WHERE a.c_attachment_id = c.aid;

DROP TABLE tmp_mask_claim;

-- 3.  Any legacy mask still without an instrument is used by nothing (or only
-- attached to an observation as a document, which the cascade cleans up).  It
-- already reads as null through the API because it has no definition, so there
-- is nothing usable to keep.  The stored file is not reachable from here and is
-- left orphaned in the object store.

DELETE FROM t_attachment
  WHERE c_attachment_type = 'mos_mask'
    AND c_mask_instrument IS NULL;

-- 4.  Nothing has prevented a mismatched assignment until now, so parsed masks
-- can legitimately be assigned to the wrong instrument.  Clearing the reference
-- returns the observation to "mask not yet defined", which the model already
-- treats as valid rather than as an error.

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

-- 5.  Now the invariants.  The instrument is present exactly for MOS masks,
-- mirroring c_mask_name, and restricted to the three instruments that do MOS at
-- Gemini -- the mask file reader recognises exactly these, so a fourth value
-- would be meaningless.

ALTER TABLE t_attachment
  ADD CONSTRAINT t_attachment_mask_instrument_check
    CHECK ((c_attachment_type = 'mos_mask') = (c_mask_instrument IS NOT NULL)),
  ADD CONSTRAINT t_attachment_mask_instrument_mos_check
    CHECK (c_mask_instrument IS NULL OR c_mask_instrument IN ('GmosNorth', 'GmosSouth', 'Flamingos2'));

-- The column and the definition are written from the same parsed value, so this
-- catches a code bug rather than expected drift.  The ELSE arm matters: without
-- it an unmapped tag would compare against NULL and pass.  Skipped when there is
-- no definition, which is what lets a legacy row carry an inferred instrument.

ALTER TABLE t_attachment
  ADD CONSTRAINT t_attachment_mask_instrument_agrees_check
    CHECK (
      c_mask_definition IS NULL OR
      c_mask_definition->>'instrument' = CASE c_mask_instrument
                                           WHEN 'GmosNorth'  THEN 'GMOS_NORTH'
                                           WHEN 'GmosSouth'  THEN 'GMOS_SOUTH'
                                           WHEN 'Flamingos2' THEN 'FLAMINGOS2'
                                           ELSE '<unmapped>'
                                         END
    );

ALTER TABLE t_attachment
  ADD CONSTRAINT t_attachment_mask_instrument_unique
    UNIQUE (c_program_id, c_attachment_id, c_attachment_type, c_mask_instrument);

-- 6.  Widen each MOS mode's mask foreign key by the instrument.  The mode's own
-- instrument is fixed and non-null, and a null mask id still skips the check
-- under default match semantics, so "no mask assigned" stays valid.  The
-- constraint names are unchanged: the services recognise the violation by name.

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
