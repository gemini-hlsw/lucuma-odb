-- IGRINS-2 long slit adopts the SlitTelescopeConfigs model mirroring the Flamingos2
-- and GNIRS long slit storage: a fixed default set of telescope configs with an
-- optional explicit (slit_offset_mode, telescope_configs) override.
--
-- The per-observation offset mode preset (c_offset_mode) goes away: the shape of an
-- explicit override now carries the mode. The static config keeps its own
-- c_offset_mode, seeded from the effective mode in the view.

-- New columns: explicit SlitTelescopeConfigs (discriminant + JSON blob).
ALTER TABLE t_igrins_2_long_slit
  ADD COLUMN c_slit_offset_mode  d_tag NULL REFERENCES t_slit_offset_mode(c_tag),
  ADD COLUMN c_telescope_configs text  NULL;

-- Migrate existing CSV offsets (p1,q1,p2,q2,... arcsec, variable length) into an explicit
-- override, preserving the previous auto-derived guide state: an offset was guided iff
-- p == 0 and |q| <= 2.5 arcsec (slit length 5").
--
-- The shape follows the row's preset: nod-along-slit rows become AlongSlit configs (q only,
-- p was constrained to 0 by a trigger), nod-to-sky rows become ToSky configs.
WITH parsed AS (
  SELECT
    m.c_observation_id,
    COALESCE(m.c_offset_mode, 'nod_along_slit') AS mode,
    string_agg(
      CASE COALESCE(m.c_offset_mode, 'nod_along_slit')
        WHEN 'nod_along_slit' THEN
          '{"q":{"microarcseconds":' ||
          round(v[i + 1]::numeric * 1000000)::bigint::text ||
          '},"guiding":"' || g.state || '"}'
        ELSE
          '{"offset":{"p":{"microarcseconds":' ||
          round(v[i]::numeric * 1000000)::bigint::text ||
          '},"q":{"microarcseconds":' ||
          round(v[i + 1]::numeric * 1000000)::bigint::text ||
          '}},"guiding":"' || g.state || '"}'
      END,
      ',' ORDER BY i
    ) AS configs
  FROM t_igrins_2_long_slit m
  CROSS JOIN LATERAL string_to_array(m.c_spatial_offsets, ',') AS v
  CROSS JOIN LATERAL generate_series(1, array_length(v, 1) - 1, 2) AS i
  CROSS JOIN LATERAL (
    SELECT CASE WHEN v[i]::numeric = 0 AND abs(v[i + 1]::numeric) <= 2.5
                THEN 'ENABLED' ELSE 'DISABLED' END AS state
  ) g
  WHERE m.c_spatial_offsets IS NOT NULL
    AND m.c_spatial_offsets <> ''
  GROUP BY m.c_observation_id, m.c_offset_mode
)
UPDATE t_igrins_2_long_slit m
SET c_slit_offset_mode  = p.mode,
    c_telescope_configs = '[' || p.configs || ']'
FROM parsed p
WHERE m.c_observation_id = p.c_observation_id;

-- Rows that relied on the nod-to-sky preset for their defaults have no explicit offsets to
-- migrate, but the preset is going away — materialize those defaults as an explicit override
-- so they don't silently fall back to the nod-along-slit default.
-- ATTENTION: duplicated from lucuma-core igrins2.NodToSkyDefaultTelescopeConfigs. Keep in sync.
UPDATE t_igrins_2_long_slit
SET c_slit_offset_mode  = 'nod_to_sky',
    c_telescope_configs =
      '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"},'  ||
       '{"offset":{"p":{"microarcseconds":10000000},"q":{"microarcseconds":10000000}},"guiding":"DISABLED"},' ||
       '{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"}]'
WHERE c_offset_mode = 'nod_to_sky'
  AND c_telescope_configs IS NULL;

-- Flush the deferred FK (c_observation_id) trigger events queued by the UPDATEs so the
-- following ALTER TABLE is not blocked by "pending trigger events" (55006).
SET CONSTRAINTS ALL IMMEDIATE;

DROP VIEW v_igrins_2_long_slit;

-- The p = 0 rule for nod-along-slit is now structural: AlongSlit configs carry only q.
DROP TRIGGER IF EXISTS check_igrins2_nod_along_slit_offsets_trigger ON t_igrins_2_long_slit;
DROP FUNCTION IF EXISTS check_igrins2_nod_along_slit_offsets();

-- The observation-edit event trigger is column-scoped, so it must be re-pointed at the new
-- columns rather than dropped with them (V1111).
DROP TRIGGER ch_observation_edit_igrins2_offsets_trigger ON t_igrins_2_long_slit;

-- The generated c_mode_key column keys on c_offset_mode, and v_all_modes /
-- v_observing_mode_group read it. A generated column's expression cannot be altered in
-- place, so drop the dependents, rebuild the column, and put them back (cf. V1190).
DROP VIEW v_observing_mode_group;
DROP VIEW v_all_modes;
DROP TRIGGER observing_mode_key_trigger ON t_igrins_2_long_slit;
ALTER TABLE t_igrins_2_long_slit DROP COLUMN c_mode_key;

ALTER TABLE t_igrins_2_long_slit
  DROP CONSTRAINT IF EXISTS check_igrins2_offsets_format;
ALTER TABLE t_igrins_2_long_slit
  DROP COLUMN c_spatial_offsets,
  DROP COLUMN c_offset_mode;

-- Send an observation update event when the telescope configs change.
CREATE TRIGGER ch_observation_edit_igrins2_offsets_trigger
AFTER UPDATE OF c_slit_offset_mode, c_telescope_configs ON t_igrins_2_long_slit
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

-- Regenerate the mode key off the explicit slit offset mode. The grouping function already
-- coalesces a NULL mode to 'nod_along_slit', which is exactly the new default, so every row
-- keeps the key it had before this migration (the backfill above materialized nod_to_sky).
-- The explicit configs themselves stay out of the key, as the offsets did before.
ALTER TABLE t_igrins_2_long_slit
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_igrins_2_long_slit_mode_group(
      c_program_id,
      c_observing_mode_type,
      c_slit_offset_mode,
      c_save_svc_images
    )
  ) STORED;

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_igrins_2_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

CREATE VIEW v_all_modes AS
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_ghost_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_igrins_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gnirs_spectroscopy;

CREATE VIEW v_observing_mode_group AS
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id
  FROM
    v_all_modes m
  JOIN t_observation o USING (c_observation_id)
  GROUP BY
    m.c_mode_key,
    o.c_program_id;

-- Explicit configs are both-or-neither (a SlitTelescopeConfigs override needs both).
ALTER TABLE t_igrins_2_long_slit
  ADD CONSTRAINT igrins2_explicit_configs_check
    CHECK ((c_telescope_configs IS NULL) = (c_slit_offset_mode IS NULL));

-- Rebuild the view with a fixed default and effective = COALESCE(explicit, default).
CREATE VIEW v_igrins_2_long_slit AS
  SELECT
    m.*,
    d.c_slit_offset_mode_default,
    d.c_telescope_configs_default,
    COALESCE(m.c_slit_offset_mode,  d.c_slit_offset_mode_default)  AS c_slit_offset_mode_effective,
    COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
  FROM t_igrins_2_long_slit m
  CROSS JOIN LATERAL (
    SELECT
      -- Default slit offset mode (shape): nod along slit.
      'nod_along_slit'::varchar AS c_slit_offset_mode_default,
      -- Default telescope configs JSON (transport codec): the ABBA nod-along-slit pattern
      -- (q = -1.25, +1.25, +1.25, -1.25 arcsec, all guided).
      -- ATTENTION: duplicated from lucuma-core igrins2.NodAlongSlitDefaultTelescopeConfigs.
      -- Keep in sync.
      '[{"q":{"microarcseconds":-1250000},"guiding":"ENABLED"},{"q":{"microarcseconds":1250000},"guiding":"ENABLED"},{"q":{"microarcseconds":1250000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1250000},"guiding":"ENABLED"}]' AS c_telescope_configs_default
  ) d;
