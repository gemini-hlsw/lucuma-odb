-- Number of passes over the telescope configs (nod-and-read).

ALTER TABLE t_scorpio_long_slit
  ADD COLUMN c_repeats int NOT NULL DEFAULT 1 CHECK (c_repeats > 0);

-- The view selects m.*, which is expanded when the view is created.
DROP VIEW v_scorpio_long_slit;

CREATE VIEW v_scorpio_long_slit AS
SELECT
  m.*,
  d.c_slit_offset_mode_default,
  d.c_telescope_configs_default,
  COALESCE(m.c_slit_offset_mode,  d.c_slit_offset_mode_default)  AS c_slit_offset_mode_effective,
  COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
FROM t_scorpio_long_slit m
CROSS JOIN LATERAL (
  SELECT
    'nod_along_slit'::varchar AS c_slit_offset_mode_default,
    -- ATTENTION: duplicated from lucuma-core scorpio.NodAlongSlitDefaultTelescopeConfigs
    -- (q = 0, -15, +15, 0 arcsec, all guided).  Keep in sync.
    '[{"q":{"microarcseconds":0},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":0},"guiding":"ENABLED"}]'::text AS c_telescope_configs_default
) d;
