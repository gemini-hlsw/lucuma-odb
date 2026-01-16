DO $$
DECLARE
  t text;
BEGIN
  FOREACH t IN ARRAY ARRAY[
    't_gmos_north_imaging',
    't_gmos_south_imaging',
    't_offset_generator',
    't_gmos_north_imaging_filter',
    't_gmos_south_imaging_filter'
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