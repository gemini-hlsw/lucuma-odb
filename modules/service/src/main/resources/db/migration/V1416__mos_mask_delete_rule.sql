-- A daytime calibration set may point at a mask in another program.  It must
-- not block that mask's owner from deleting it: the set loses its mask instead.
-- Stored steps keep blocking the delete, as before.  The index keeps an
-- attachment delete from scanning the setup tables.
DO $$
DECLARE
  t text;
  c text;
BEGIN
  FOREACH t IN ARRAY ARRAY[
    't_gmos_north_daytime_calibration_setup',
    't_gmos_south_daytime_calibration_setup',
    't_flamingos_2_daytime_calibration_setup'
  ]
  LOOP
    SELECT con.conname INTO STRICT c
    FROM pg_constraint con
    JOIN pg_attribute  att ON att.attrelid = con.conrelid AND att.attnum = ANY (con.conkey)
    WHERE con.conrelid  = t::regclass
      AND con.contype   = 'f'
      AND con.confrelid = 't_attachment'::regclass
      AND att.attname   = 'c_fpu_custom_mask_attachment_id';

    EXECUTE format('ALTER TABLE %I DROP CONSTRAINT %I', t, c);
    EXECUTE format($f$
      ALTER TABLE %I
        ADD CONSTRAINT %I
        FOREIGN KEY (c_fpu_custom_mask_attachment_id)
        REFERENCES t_attachment(c_attachment_id)
        ON DELETE SET NULL
    $f$, t, substring(t from 3) || '_mask_fkey');
    EXECUTE format(
      'CREATE INDEX %I ON %I (c_fpu_custom_mask_attachment_id)',
      'i_' || substring(t from 3) || '_mask', t
    );
  END LOOP;
END $$;
