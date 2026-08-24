-- GMOS long slit and MOS move from a q-only CSV of spatial offsets to telescope
-- configurations, so a position can carry a p component and its own guide state.
--
-- The two modes take different shapes, following lucuma-core 7db49d94:
--   long slit -> SlitTelescopeConfigs, stored as (c_slit_offset_mode, JSON)
--   MOS       -> a plain [TelescopeConfig] list, stored as JSON alone
--
-- Unlike Flamingos2 and GNIRS there are no `_default` / `_effective` columns: the
-- GMOS defaults are constants rather than functions of other columns, and the
-- mappings already serve them from Scala.  The tables hold only the explicit
-- value and Scala resolves `explicit.getOrElse(default)`.

ALTER TABLE t_gmos_north_long_slit
  ADD COLUMN c_slit_offset_mode  d_tag NULL REFERENCES t_slit_offset_mode(c_tag),
  ADD COLUMN c_telescope_configs text  NULL;

ALTER TABLE t_gmos_south_long_slit
  ADD COLUMN c_slit_offset_mode  d_tag NULL REFERENCES t_slit_offset_mode(c_tag),
  ADD COLUMN c_telescope_configs text  NULL;

ALTER TABLE t_gmos_north_mos
  ADD COLUMN c_telescope_configs text NULL;

ALTER TABLE t_gmos_south_mos
  ADD COLUMN c_telescope_configs text NULL;

-- Migrate the existing offsets, preserving every one of them as an along-slit nod.
-- c_offsets is a CSV of q values in arcsec, of any length, and every position was
-- guided: GMOS never derived a guide state, so the conversion is exact rather than
-- a best guess, and p is zero throughout because a q-only offset is along the slit.
--
-- A NULL c_offsets means "no explicit value" and stays NULL, so those rows keep
-- falling through to the default.  The defaults carry the same meaning as before:
-- long slit's is the same 0/+15/-15 nod, and MOS's single guided zero is what an
-- empty offset list already generated.
--
-- Long slit keeps its along-slit shape, whose JSON carries q alone.
UPDATE t_gmos_north_long_slit
SET c_slit_offset_mode  = 'nod_along_slit',
    c_telescope_configs = (
      SELECT '[' || string_agg(
               '{"q":{"microarcseconds":' ||
               round(q::numeric * 1000000)::bigint::text ||
               '},"guiding":"ENABLED"}', ',' ORDER BY ord) || ']'
        FROM unnest(string_to_array(c_offsets, ',')) WITH ORDINALITY AS t(q, ord)
    )
WHERE c_offsets IS NOT NULL AND c_offsets <> '';

UPDATE t_gmos_south_long_slit
SET c_slit_offset_mode  = 'nod_along_slit',
    c_telescope_configs = (
      SELECT '[' || string_agg(
               '{"q":{"microarcseconds":' ||
               round(q::numeric * 1000000)::bigint::text ||
               '},"guiding":"ENABLED"}', ',' ORDER BY ord) || ']'
        FROM unnest(string_to_array(c_offsets, ',')) WITH ORDINALITY AS t(q, ord)
    )
WHERE c_offsets IS NOT NULL AND c_offsets <> '';

-- MOS positions are full offsets and have no slit offset mode, so the same
-- along-slit nod is written with p explicitly zero.
UPDATE t_gmos_north_mos
SET c_telescope_configs = (
      SELECT '[' || string_agg(
               '{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":' ||
               round(q::numeric * 1000000)::bigint::text ||
               '}},"guiding":"ENABLED"}', ',' ORDER BY ord) || ']'
        FROM unnest(string_to_array(c_offsets, ',')) WITH ORDINALITY AS t(q, ord)
    )
WHERE c_offsets IS NOT NULL AND c_offsets <> '';

UPDATE t_gmos_south_mos
SET c_telescope_configs = (
      SELECT '[' || string_agg(
               '{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":' ||
               round(q::numeric * 1000000)::bigint::text ||
               '}},"guiding":"ENABLED"}', ',' ORDER BY ord) || ']'
        FROM unnest(string_to_array(c_offsets, ',')) WITH ORDINALITY AS t(q, ord)
    )
WHERE c_offsets IS NOT NULL AND c_offsets <> '';

-- Flush the deferred FK (c_observation_id) trigger events queued by the UPDATEs so the
-- following ALTER TABLE is not blocked by "pending trigger events" (55006).
SET CONSTRAINTS ALL IMMEDIATE;

-- Drop everything that keys on c_offsets.  A generated column's expression cannot be
-- altered in place, so c_mode_key is rebuilt rather than changed (cf. V1222).  The
-- column itself stays; see the note below.
DROP VIEW v_gmos_north_long_slit;
DROP VIEW v_gmos_south_long_slit;
DROP VIEW v_gmos_north_mos;
DROP VIEW v_gmos_south_mos;
DROP VIEW v_observing_mode_group;
DROP VIEW v_all_modes;

DROP TRIGGER observing_mode_key_trigger ON t_gmos_north_long_slit;
DROP TRIGGER observing_mode_key_trigger ON t_gmos_south_long_slit;
DROP TRIGGER observing_mode_key_trigger ON t_gmos_north_mos;
DROP TRIGGER observing_mode_key_trigger ON t_gmos_south_mos;

ALTER TABLE t_gmos_north_long_slit DROP COLUMN c_mode_key;
ALTER TABLE t_gmos_south_long_slit DROP COLUMN c_mode_key;
ALTER TABLE t_gmos_north_mos       DROP COLUMN c_mode_key;
ALTER TABLE t_gmos_south_mos       DROP COLUMN c_mode_key;

-- c_offsets is deliberately left in place. Nothing reads or writes it any more --
-- the mode keys, views, services and mappings all work off c_telescope_configs now --
-- but keeping the old values means the conversion above can be checked against them,
-- and reversed, without a restore. New rows will simply leave it NULL. We will drop
-- the columns in a future PR.
--
-- Drop it in a later migration once the telescope configs have proven themselves:
--
--   ALTER TABLE t_gmos_north_long_slit
--     DROP CONSTRAINT IF EXISTS gmos_north_long_slit_offset_format,
--     DROP COLUMN c_offsets;
--   ALTER TABLE t_gmos_south_long_slit
--     DROP CONSTRAINT IF EXISTS gmos_south_long_slit_offset_format,
--     DROP COLUMN c_offsets;
--   ALTER TABLE t_gmos_north_mos
--     DROP CONSTRAINT IF EXISTS gmos_north_mos_offset_format,
--     DROP COLUMN c_offsets;
--   ALTER TABLE t_gmos_south_mos
--     DROP CONSTRAINT IF EXISTS gmos_south_mos_offset_format,
--     DROP COLUMN c_offsets;

-- A long slit override needs both halves; MOS has no mode to pair with.
ALTER TABLE t_gmos_north_long_slit
  ADD CONSTRAINT gmos_north_long_slit_telescope_configs_check
    CHECK ((c_telescope_configs IS NULL) = (c_slit_offset_mode IS NULL));
ALTER TABLE t_gmos_south_long_slit
  ADD CONSTRAINT gmos_south_long_slit_telescope_configs_check
    CHECK ((c_telescope_configs IS NULL) = (c_slit_offset_mode IS NULL));

-- Regenerate the mode keys off the telescope configs.  Long slit folds in the
-- slit offset mode: two configurations with the same positions but different
-- shapes are different configurations.
ALTER TABLE t_gmos_north_long_slit
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
      'gn',
      c_program_id,
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_fpu,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      COALESCE(c_slit_offset_mode, '') || ':' || COALESCE(c_telescope_configs, '')
    )
  ) STORED;

ALTER TABLE t_gmos_south_long_slit
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
      'gs',
      c_program_id,
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_fpu,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      COALESCE(c_slit_offset_mode, '') || ':' || COALESCE(c_telescope_configs, '')
    )
  ) STORED;

ALTER TABLE t_gmos_north_mos
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
      'gn',
      c_program_id,
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_slit_width,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      c_telescope_configs
    )
  ) STORED;

ALTER TABLE t_gmos_south_mos
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
      'gs',
      c_program_id,
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_slit_width,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      c_telescope_configs
    )
  ) STORED;

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gmos_north_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gmos_south_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gmos_north_mos
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gmos_south_mos
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

-- Rebuild the mode views unchanged apart from the dropped column.
CREATE VIEW v_gmos_north_long_slit AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_north_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default,

  CASE
    WHEN o.c_calibration_role = 'spectrophotometric'::e_calibration_role THEN 'Stamp'::e_gmos_long_slit_acquisition_roi
    WHEN m.c_roi              = 'CentralSpectrum'                        THEN 'Ccd2Stamp'::e_gmos_long_slit_acquisition_roi
    ELSE 'Ccd2'::e_gmos_long_slit_acquisition_roi
  END AS c_acquisition_roi_default

FROM t_gmos_north_long_slit m
INNER JOIN t_observation o ON o.c_observation_id = m.c_observation_id;

CREATE VIEW v_gmos_south_long_slit AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_south_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default,

  CASE
    WHEN o.c_calibration_role = 'spectrophotometric'::e_calibration_role THEN 'Stamp'::e_gmos_long_slit_acquisition_roi
    WHEN m.c_roi              = 'CentralSpectrum'                        THEN 'Ccd2Stamp'::e_gmos_long_slit_acquisition_roi
    ELSE 'Ccd2'::e_gmos_long_slit_acquisition_roi
  END AS c_acquisition_roi_default

FROM t_gmos_south_long_slit m
INNER JOIN t_observation o ON o.c_observation_id = m.c_observation_id;

CREATE VIEW v_gmos_north_mos AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_north_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default
FROM t_gmos_north_mos m;

CREATE VIEW v_gmos_south_mos AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_south_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default
FROM t_gmos_south_mos m;

CREATE VIEW v_all_modes AS
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_mos
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_ghost_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_mos
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_mos
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
