-- The Daytime Calibration Set tables (V1361) invalidate obscalc but never
-- emitted the observationEdit notification, so clients did not refresh the
-- generated sequence after a set changed.  The shared notify function reads
-- c_program_id from the changed row, which these tables do not carry, so this
-- variant looks it up from the observation instead.  The mode table itself
-- (t_daytime_calibration) had the same gap.
CREATE OR REPLACE FUNCTION ch_observation_edit_by_observation()
  RETURNS trigger AS $$
DECLARE
  rec record;
  pid d_program_id;
BEGIN
  rec := COALESCE(NEW, OLD);
  IF TG_OP <> 'UPDATE' OR ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    SELECT c_program_id INTO pid FROM t_observation WHERE c_observation_id = rec.c_observation_id;
    IF pid IS NOT NULL THEN
      PERFORM pg_notify('ch_observation_edit', rec.c_observation_id || ',' || pid || ',' || 'UPDATE');
    END IF;
  END IF;
  RETURN rec;
END;
$$ LANGUAGE plpgsql;

DO $$
DECLARE
  t text;
BEGIN
  FOREACH t IN ARRAY ARRAY[
    't_daytime_calibration',
    't_daytime_calibration_set',
    't_gmos_north_daytime_calibration_setup',
    't_gmos_south_daytime_calibration_setup'
  ]
  LOOP
    EXECUTE format($f$
      CREATE TRIGGER %s_edit_notify_trigger
      AFTER INSERT OR UPDATE OR DELETE ON %I
      FOR EACH ROW EXECUTE FUNCTION ch_observation_edit_by_observation()
    $f$, substring(t from 3), t);
  END LOOP;
END;
$$;
