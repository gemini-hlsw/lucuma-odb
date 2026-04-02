CREATE TYPE e_visit_origin AS ENUM(
  'slew',
  'observe'
);

ALTER TABLE t_visit
  ADD COLUMN c_origin e_visit_origin NOT NULL DEFAULT 'observe';

-- Transition the tables to versions with a single entry per observation id
-- and no c_visit_id column.
DO $$
DECLARE
  t_name text;
  t_names text[] := array[
    't_flamingos_2_static',
    't_gmos_north_static',
    't_gmos_south_static',
    't_igrins_2_static'
  ];
  deleted_count bigint;
BEGIN
  FOREACH t_name IN ARRAY t_names LOOP
    EXECUTE format($f$
      WITH ranked AS (
        SELECT
          c_static_id,
          ROW_NUMBER() OVER (
            PARTITION BY c_observation_id
            ORDER BY
              (c_visit_id IS NULL) DESC,
              c_static_id DESC
          ) AS row_num
        FROM %I
      )
      DELETE FROM %I t
      USING ranked r
      WHERE t.c_static_id = r.c_static_id
        AND r.row_num > 1 $f$,
      t_name,
      t_name
    );

    GET DIAGNOSTICS deleted_count = ROW_COUNT;

    RAISE NOTICE 'Deleted % duplicate rows from %', deleted_count, t_name;

    EXECUTE format('ALTER TABLE %I DROP COLUMN c_visit_id', t_name);
  END LOOP;
END $$;