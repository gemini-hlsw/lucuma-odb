-- The user-defined tellurics change (PR #3176) added a `count` field to the
-- TelluricType schema type and its JSON codec (tellurictype.scala), and renamed
-- the MANUAL tag to EXPLICIT_SPECTRAL_TYPES, but left the stored jsonb
-- untouched.  The mappings expose c_telluric_type through SqlJson, which needs
-- every schema field present in the stored object: rows written before that
-- change (and the column defaults) lack `count`, so selecting it fails with
-- "No mapping for field 'count' for type TelluricType".  MANUAL rows, if any,
-- no longer decode at all.

DO $$
DECLARE
  t text;
BEGIN
  FOREACH t IN ARRAY ARRAY[
    't_flamingos_2_long_slit',
    't_flamingos_2_mos',
    't_gnirs_spectroscopy',
    't_igrins_2_long_slit'
  ]
  LOOP

    EXECUTE format($f$
      ALTER TABLE %I
        ALTER COLUMN c_telluric_type
        SET DEFAULT '{"tag":"HOT","starTypes":null,"count":null}'::jsonb
    $f$, t);

    -- Add the missing keys.  The value is unchanged so there is no obscalc
    -- result to invalidate.
    EXECUTE format('ALTER TABLE %I DISABLE TRIGGER USER', t);

    EXECUTE format($f$
      UPDATE %I
         SET c_telluric_type = '{"starTypes":null,"count":null}'::jsonb || c_telluric_type
       WHERE NOT (c_telluric_type ?& ARRAY['starTypes', 'count'])
    $f$, t);

    EXECUTE format('ALTER TABLE %I ENABLE TRIGGER USER', t);

    -- MANUAL becomes EXPLICIT_SPECTRAL_TYPES.  Triggers are enabled so obscalc
    -- for these observations, which could not decode their telluric type, is
    -- recalculated.  This must follow the ALTER TABLEs above: the update queues
    -- deferred consistency trigger events, and a table with pending trigger
    -- events cannot be altered.
    EXECUTE format($f$
      UPDATE %I
         SET c_telluric_type = c_telluric_type || '{"tag":"EXPLICIT_SPECTRAL_TYPES"}'::jsonb
       WHERE upper(c_telluric_type->>'tag') = 'MANUAL'
    $f$, t);

  END LOOP;
END;
$$;
