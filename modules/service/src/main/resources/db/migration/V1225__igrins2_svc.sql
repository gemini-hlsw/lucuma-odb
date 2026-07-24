-- IGRINS-2 long slit now supports a Slit-Viewing Camera (SVC) acquisition sub-config:
-- an SVC exposure time and a list of SVC telescope dither positions, stored alongside the
-- existing c_save_svc_images toggle.

ALTER TABLE t_igrins_2_long_slit
  ADD COLUMN c_svc_exposure          interval NULL,
  ADD COLUMN c_svc_telescope_configs text     NULL;

-- SVC exposure, when explicitly set, must respect the IGRINS-2 detector limits
ALTER TABLE t_igrins_2_long_slit
  ADD CONSTRAINT igrins2_svc_exposure_check
    CHECK (c_svc_exposure IS NULL
           OR (c_svc_exposure >= interval '3080 milliseconds'
               AND c_svc_exposure <= interval '600 seconds'));

-- c_save_svc_images is now always set
UPDATE t_igrins_2_long_slit SET c_save_svc_images = false WHERE c_save_svc_images IS NULL;
ALTER TABLE t_igrins_2_long_slit ALTER COLUMN c_save_svc_images SET DEFAULT false;
ALTER TABLE t_igrins_2_long_slit ALTER COLUMN c_save_svc_images SET NOT NULL;

-- Rebuild view
DROP VIEW v_igrins_2_long_slit;

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

-- Notify observers when the SVC sub-config changes
CREATE TRIGGER ch_observation_edit_igrins2_svc_trigger
AFTER UPDATE OF c_svc_exposure, c_svc_telescope_configs ON t_igrins_2_long_slit
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();
