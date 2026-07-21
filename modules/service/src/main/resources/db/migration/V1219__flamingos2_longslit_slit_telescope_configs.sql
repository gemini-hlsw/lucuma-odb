-- Flamingos2 long slit adopts the SlitTelescopeConfigs model mirroring the GNIRS 
-- long slit storage:
-- a fixed default set of telescope configs with an optional explicit
-- (slit_offset_mode, telescope_configs) override.

-- New columns: explicit SlitTelescopeConfigs (discriminant + JSON blob).
ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_slit_offset_mode  d_tag NULL REFERENCES t_slit_offset_mode(c_tag),
  ADD COLUMN c_telescope_configs text  NULL;

-- Migrate existing CSV offsets (p1,q1,...,p4,q4 arcsec) into an explicit ToSky
-- override, preserving the previous auto-derived guide state: an offset was guided
-- iff p == 0 and |q| <= 54 arcsec (slit length 108").
UPDATE t_flamingos_2_long_slit
SET c_slit_offset_mode = 'nod_to_sky',
    c_telescope_configs =
  '[' ||
  '{"offset":{"p":{"microarcseconds":' || round(split_part(c_offsets, ',', 1)::numeric * 1000000)::bigint::text ||
  '},"q":{"microarcseconds":' || round(split_part(c_offsets, ',', 2)::numeric * 1000000)::bigint::text ||
  '}},"guiding":"' ||
  CASE WHEN split_part(c_offsets, ',', 1)::numeric = 0 AND abs(split_part(c_offsets, ',', 2)::numeric) <= 54
       THEN 'ENABLED' ELSE 'DISABLED' END || '"},' ||
  '{"offset":{"p":{"microarcseconds":' || round(split_part(c_offsets, ',', 3)::numeric * 1000000)::bigint::text ||
  '},"q":{"microarcseconds":' || round(split_part(c_offsets, ',', 4)::numeric * 1000000)::bigint::text ||
  '}},"guiding":"' ||
  CASE WHEN split_part(c_offsets, ',', 3)::numeric = 0 AND abs(split_part(c_offsets, ',', 4)::numeric) <= 54
       THEN 'ENABLED' ELSE 'DISABLED' END || '"},' ||
  '{"offset":{"p":{"microarcseconds":' || round(split_part(c_offsets, ',', 5)::numeric * 1000000)::bigint::text ||
  '},"q":{"microarcseconds":' || round(split_part(c_offsets, ',', 6)::numeric * 1000000)::bigint::text ||
  '}},"guiding":"' ||
  CASE WHEN split_part(c_offsets, ',', 5)::numeric = 0 AND abs(split_part(c_offsets, ',', 6)::numeric) <= 54
       THEN 'ENABLED' ELSE 'DISABLED' END || '"},' ||
  '{"offset":{"p":{"microarcseconds":' || round(split_part(c_offsets, ',', 7)::numeric * 1000000)::bigint::text ||
  '},"q":{"microarcseconds":' || round(split_part(c_offsets, ',', 8)::numeric * 1000000)::bigint::text ||
  '}},"guiding":"' ||
  CASE WHEN split_part(c_offsets, ',', 7)::numeric = 0 AND abs(split_part(c_offsets, ',', 8)::numeric) <= 54
       THEN 'ENABLED' ELSE 'DISABLED' END || '"}' ||
  ']'
WHERE c_offsets IS NOT NULL;

-- Flush the deferred FK (c_observation_id) trigger events queued by the UPDATE so the
-- following ALTER TABLE is not blocked by "pending trigger events" (55006).
SET CONSTRAINTS ALL IMMEDIATE;

DROP VIEW v_flamingos_2_long_slit;

ALTER TABLE t_flamingos_2_long_slit
  DROP CONSTRAINT IF EXISTS flamingos2_offsets_format;
ALTER TABLE t_flamingos_2_long_slit
  DROP COLUMN c_offsets;

-- Explicit configs are both-or-neither (a SlitTelescopeConfigs override needs both).
ALTER TABLE t_flamingos_2_long_slit
  ADD CONSTRAINT flamingos2_explicit_configs_check
    CHECK ((c_telescope_configs IS NULL) = (c_slit_offset_mode IS NULL));

-- Rebuild the view with a fixed default and effective = COALESCE(explicit, default).
CREATE VIEW v_flamingos_2_long_slit AS
  SELECT
    m.*,
    (
      SELECT af.c_tag
        FROM t_f2_filter af
        JOIN t_f2_filter sf ON sf.c_tag = m.c_filter
        WHERE af.c_is_acquisition_filter
        ORDER BY abs(af.c_wavelength - sf.c_wavelength)
        LIMIT 1
    ) AS c_acquisition_filter_default,
    d.c_slit_offset_mode_default,
    d.c_telescope_configs_default,
    COALESCE(m.c_slit_offset_mode,  d.c_slit_offset_mode_default)  AS c_slit_offset_mode_effective,
    COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
  FROM t_flamingos_2_long_slit m
  CROSS JOIN LATERAL (
    SELECT
      -- Default slit offset mode (shape): the Telluric preset is nod-along-slit.
      'nod_along_slit'::varchar AS c_slit_offset_mode_default,
      -- Default telescope configs JSON (transport codec): the Telluric nod-along-slit
      -- pattern (q = +15, -15, -15, +15 arcsec, all guided).
      -- ATTENTION: duplicated from lucuma-core flamingos2.defaultSlitTelescopeConfigs
      -- (Flamingos2SlitOffsetPreset.Telluric). Keep in sync.
      '[{"q":{"microarcseconds":15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":15000000},"guiding":"ENABLED"}]' AS c_telescope_configs_default
  ) d;
