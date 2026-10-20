-- The ITC now reports the read mode it computed with, and the sequence generators
-- require it. Results frozen before that carry none; fill them in with the rule the
-- generators derived the read mode from until now, so an executing observation keeps
-- the read modes its sequence was generated with. Rows that are not frozen are wiped
-- by the ITC version update and recomputed.

-- Adds the read mode under `key` to every ITC result in `j` (an object carrying an
-- `exposureTime`) that has no read mode field yet, recursing through the result sets.
CREATE OR REPLACE FUNCTION itc_backfill_read_mode(j jsonb, key text, rule text, imaging_default text)
  RETURNS jsonb AS $$
DECLARE
  us   bigint;
  mode text;
BEGIN
  CASE jsonb_typeof(j)
    WHEN 'object' THEN
      IF j ? 'exposureTime' AND NOT (j ? 'gnirsReadMode' OR j ? 'flamingos2ReadMode') THEN
        us   := (j -> 'exposureTime' ->> 'microseconds')::bigint;
        mode := CASE rule
          WHEN 'gnirs' THEN
            CASE WHEN us <    600000 THEN 'VERY_BRIGHT'
                 WHEN us <= 20000000 THEN 'BRIGHT'
                 WHEN us <= 60000000 THEN 'FAINT'
                 ELSE                     'VERY_FAINT'
            END
          WHEN 'f2_spectroscopy' THEN
            CASE WHEN us >= 85000000 THEN 'FAINT'
                 WHEN us >= 21000000 THEN 'MEDIUM'
                 ELSE                     'BRIGHT'
            END
          WHEN 'f2_imaging' THEN imaging_default
        END;
        RETURN j || jsonb_build_object(key, mode);
      ELSE
        RETURN (
          SELECT COALESCE(jsonb_object_agg(e.k, itc_backfill_read_mode(e.v, key, rule, imaging_default)), '{}'::jsonb)
            FROM jsonb_each(j) AS e(k, v)
        );
      END IF;
    WHEN 'array' THEN
      RETURN (
        SELECT COALESCE(jsonb_agg(itc_backfill_read_mode(a.e, key, rule, imaging_default)), '[]'::jsonb)
          FROM jsonb_array_elements(j) AS a(e)
      );
    ELSE
      RETURN j;
  END CASE;
END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;

-- The rule per observing mode. Flamingos 2 imaging science took the configuration's
-- default read mode rather than deriving it (V1356 left executing observations on their
-- old default); an explicit read mode overrides either way.
UPDATE t_itc_result r SET
  c_science_results     = itc_backfill_read_mode(r.c_science_results,     k.key, k.science_rule,     COALESCE(upper(i.c_read_mode_default), '')),
  c_acquisition_results = itc_backfill_read_mode(r.c_acquisition_results, k.key, k.acquisition_rule, COALESCE(upper(i.c_read_mode_default), ''))
FROM t_observation o
LEFT JOIN t_flamingos_2_imaging i ON i.c_observation_id = o.c_observation_id
JOIN (VALUES
  ('gnirs_long_slit',       'gnirsReadMode',      'gnirs',           'gnirs'),
  ('gnirs_ifu',             'gnirsReadMode',      'gnirs',           'gnirs'),
  ('gnirs_imaging',         'gnirsReadMode',      'gnirs',           'gnirs'),
  ('flamingos_2_long_slit', 'flamingos2ReadMode', 'f2_spectroscopy', 'f2_spectroscopy'),
  ('flamingos_2_mos',       'flamingos2ReadMode', 'f2_spectroscopy', 'f2_spectroscopy'),
  ('flamingos_2_imaging',   'flamingos2ReadMode', 'f2_imaging',      'f2_spectroscopy')
) AS k(mode, key, science_rule, acquisition_rule) ON o.c_observing_mode_type::text = k.mode
WHERE r.c_is_frozen
  AND r.c_program_id     = o.c_program_id
  AND r.c_observation_id = o.c_observation_id;

DROP FUNCTION itc_backfill_read_mode(jsonb, text, text, text);
