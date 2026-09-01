-- Config edits on these mode tables never invalidated obscalc: V0988 installed
-- obsid_obscalc_invalidate on the mode tables that existed at the time, and
-- these later modes were registered without it. An edit that touches only the
-- mode table (e.g. changing the telluric type on Flamingos2 MOS) therefore
-- left obscalc -- and everything cascading from it -- stale.

DO $$
DECLARE
  t text;
BEGIN
  FOREACH t IN ARRAY ARRAY[
    't_flamingos_2_mos',
    't_gmos_north_mos',
    't_gmos_south_mos',
    't_gmos_north_ifu',
    't_gmos_south_ifu',
    't_gnirs_imaging'
  ]
  LOOP
    EXECUTE format($f$
      CREATE TRIGGER %I_invalidate_trigger
      AFTER INSERT OR UPDATE OR DELETE ON %I
      FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate()
    $f$, substring(t from 3), t);
  END LOOP;
END;
$$;

-- Install the invalidation trigger as part of mode registration, the way the
-- consistency trigger already is
--
-- With this change we'll avoid forgetting to add the invalidation trigger
-- Called for eaxmple as
-- Register mode for observing mode consistency trigger
-- SELECT register_observing_mode('gnirs_long_slit', 't_gnirs_long_slit');
--
CREATE OR REPLACE FUNCTION register_observing_mode(
  observing_mode_type e_observing_mode_type,
  mode_table_name     text
)
RETURNS void AS $$
BEGIN

  -- Add the mode to the registry table
  INSERT INTO t_observing_mode_registry (
    c_observing_mode_type,
    c_table_name
  ) VALUES (
    observing_mode_type,
    mode_table_name
  );

  BEGIN
    EXECUTE format($trig$
      CREATE CONSTRAINT TRIGGER %I
        AFTER INSERT OR UPDATE OR DELETE ON %I
        DEFERRABLE INITIALLY DEFERRED
        FOR EACH ROW EXECUTE FUNCTION check_observing_mode_consistency()
        $trig$,
      'trigger_' || mode_table_name || '_consistency',
      mode_table_name
    );
    EXCEPTION WHEN SQLSTATE '42710' THEN
      BEGIN
        RAISE NOTICE 'Ignoring duplicate consistency trigger creation.';
      END;
  END;

  BEGIN
    EXECUTE format($trig$
      CREATE TRIGGER %I
        AFTER INSERT OR UPDATE OR DELETE ON %I
        FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate()
        $trig$,
      substring(mode_table_name from 3) || '_invalidate_trigger',
      mode_table_name
    );
    EXCEPTION WHEN SQLSTATE '42710' THEN
      BEGIN
        RAISE NOTICE 'Ignoring duplicate invalidate trigger creation.';
      END;
  END;

END;
$$ LANGUAGE plpgsql;
