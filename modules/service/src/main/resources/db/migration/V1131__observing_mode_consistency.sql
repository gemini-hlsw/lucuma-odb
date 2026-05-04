-- The t_observation table contains a c_observing_mode_type discriminator.
-- This must correspond to exactly one entry in a matching mode table.  Nothing
-- guarantees this rule though, so we'll check it in a trigger function.  It
-- will work with a "mode registry" so that there is just one place to update
-- when new modes are added and so that it fails quickly if you forget.

CREATE TABLE t_observing_mode_registry (
  c_observing_mode_type e_observing_mode_type PRIMARY KEY,
  c_table_name          text                  NOT NULL UNIQUE
);

CREATE FUNCTION check_observing_mode_consistency()
RETURNS TRIGGER AS $$
DECLARE
  observation_id      d_tag;
  observing_mode_type e_observing_mode_type;
  query_sql           text;
  query_result        record;
  mode_table_name     text;
  any_found           bool;
BEGIN

  -- Which observation's observing mode was updated?
  observation_id := COALESCE(NEW.c_observation_id, OLD.c_observation_id);

  -- What is the observing mode type now, if any?
  SELECT c_observing_mode_type
    INTO observing_mode_type
    FROM t_observation
   WHERE c_observation_id = observation_id;

  -- Build a single query that returns (table_name, 0 or 1) for all mode tables
  -- at once.  The value will be 0 if the mode table does not contain the
  -- observation, and 1 if it does.  There should be at most one result with a
  -- 1 value.
  SELECT string_agg(
    format(
      'SELECT %L AS c_table_name, EXISTS(SELECT 1 FROM %I WHERE c_observation_id = %L)::int AS c_count',
      r.c_table_name, r.c_table_name, observation_id
    ),
    ' UNION ALL '
  )
  INTO query_sql
  FROM t_observing_mode_registry r;

  -- NULL discriminator (i.e., no observing mode type).  In this case there
  -- should be no results with a count that is not 0.
  IF observing_mode_type IS NULL THEN
    EXECUTE format('SELECT EXISTS(SELECT 1 FROM (%s) s WHERE c_count != 0)', query_sql) INTO any_found;
      IF any_found THEN
        RAISE EXCEPTION 'Observation % has NULL c_observing_mode_type but is referenced by a mode table', observation_id;
      END IF;
    RETURN NEW;
  END IF;

  -- Okay there was an observing mode type.  What is the matching mode table?
  SELECT c_table_name
    INTO mode_table_name
    FROM t_observing_mode_registry
   WHERE c_observing_mode_type = observing_mode_type;

  -- Check that there is an observing mode table that corresponds to the observing
  -- mode type.
  IF NOT FOUND THEN
    RAISE EXCEPTION
      'Observation % has unrecognized c_observing_mode_type "%": not present in t_observing_mode_registry',
      observation_id,
      observing_mode_type;
  END IF;

  -- Expected mode table has exactly 1 row, all others have 0; any deviation is
  -- reported with the offending table name and count.
  EXECUTE format(
    'SELECT c_table_name, c_count FROM (%s) s WHERE c_count <> CASE WHEN c_table_name = %L THEN 1 ELSE 0 END LIMIT 1',
    query_sql,
    mode_table_name
  ) INTO query_result;

  IF query_result IS NOT NULL THEN
    RAISE EXCEPTION
      'Observing mode inconsistency for observation %: discriminator is "%" (expects row in %) but % has % row(s)',
      observation_id,
      observing_mode_type,
      mode_table_name,
      query_result.c_table_name,
      query_result.c_count;
  END IF;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

-- Here we make a function that adds a new observing mode into the registry
-- and creates a trigger for it to call the consistency check.
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

  -- Add a new trigger function.
  EXECUTE format($trig$
    CREATE CONSTRAINT TRIGGER %I
      AFTER INSERT OR UPDATE OR DELETE ON %I
      DEFERRABLE INITIALLY DEFERRED
      FOR EACH ROW EXECUTE FUNCTION check_observing_mode_consistency()
      $trig$,
    'trigger_' || mode_table_name || '_consistency',
    mode_table_name
  );
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_observation_mode_consistency
  AFTER INSERT OR UPDATE OF c_observing_mode_type ON t_observation
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_observing_mode_consistency();

-- Register each observing mode so that the consistency check will be
-- performed when they are updated.
SELECT register_observing_mode('flamingos_2_long_slit', 't_flamingos_2_long_slit');
SELECT register_observing_mode('ghost_ifu',             't_ghost_ifu');
SELECT register_observing_mode('gmos_north_imaging',    't_gmos_north_imaging');
SELECT register_observing_mode('gmos_north_long_slit',  't_gmos_north_long_slit');
SELECT register_observing_mode('gmos_south_imaging',    't_gmos_south_imaging');
SELECT register_observing_mode('gmos_south_long_slit',  't_gmos_south_long_slit');
SELECT register_observing_mode('igrins_2_long_slit',    't_igrins_2_long_slit');