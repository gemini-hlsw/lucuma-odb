-- Configuration requests now record every item that needs approval (sc-9766):
--
--   * the imaging filters of GMOS (already stored), Flamingos-2 and GNIRS,
--   * the GNIRS imaging camera, and
--   * the GHOST IFU resolution mode.
--
-- `Configuration.subsumes` reinstates the filter check relaxed in sc-8036: an
-- approved imaging request covers any subset of its filters, so dropping a filter
-- needs no new approval but adding one does.  The camera and resolution mode must
-- match exactly.  All of them are therefore part of a request's identity, and join
-- the uniqueness key (see `SelectRequest`).

ALTER TABLE t_configuration_request
  ADD COLUMN c_flamingos_2_imaging_filters _d_tag NULL,
  ADD COLUMN c_gnirs_imaging_camera        d_tag  NULL REFERENCES t_gnirs_camera(c_tag),
  ADD COLUMN c_gnirs_imaging_filters       _d_tag NULL,
  ADD COLUMN c_ghost_ifu_resolution_mode   d_tag  NULL REFERENCES t_ghost_resolution_mode(c_tag);

-- A filter array is part of the key, so it must have one spelling per set: sorted
-- by tag in byte order (matching the service, which sorts on the tag string) with
-- no repeats.
CREATE FUNCTION pg_temp.canonical_tags(tags _d_tag) RETURNS _d_tag AS $$
  SELECT COALESCE(array_agg(t ORDER BY t COLLATE "C"), '{}'::_d_tag)
    FROM (SELECT DISTINCT unnest(tags) AS t) u;
$$ LANGUAGE sql IMMUTABLE;

-- Existing requests predate the new checks.  A request is not linked to any
-- observation, so the backfill below looks at the program's observations whose
-- configuration matches the request on every exactly-stored field (the target,
-- matched in Scala by radius or region, is left out).
--
-- Requests are only made for active observations (`SelectActiveNonCalibrations`),
-- so those are the ones counted, and inactive observations cannot outvote the one
-- a request was made for.  Only when no active observation matches (say, all were
-- deactivated after approval) are the inactive ones used instead, so that the
-- request keeps their values rather than an arbitrary default and they stay covered
-- if reactivated.
CREATE FUNCTION pg_temp.matching_observations(cr t_configuration_request)
  RETURNS SETOF d_observation_id AS $$
  WITH m AS (
    SELECT o.c_observation_id,
           o.c_workflow_user_state IS DISTINCT FROM 'inactive' AS active
      FROM t_observation o
     WHERE o.c_program_id          = cr.c_program_id
       AND o.c_existence           = 'present'
       AND o.c_calibration_role    IS NULL
       AND o.c_observing_mode_type = cr.c_observing_mode_type
       AND o.c_cloud_extinction    = cr.c_cloud_extinction
       AND o.c_image_quality       = cr.c_image_quality
       AND o.c_sky_background      = cr.c_sky_background
       AND o.c_water_vapor         = cr.c_water_vapor
       AND o.c_altair_mode IS NOT DISTINCT FROM cr.c_altair_mode
       AND o.c_scheduling_mode     = cr.c_scheduling_mode
  )
  SELECT c_observation_id
    FROM m
   WHERE active OR NOT EXISTS (SELECT 1 FROM m WHERE active);
$$ LANGUAGE sql STABLE;

-- Each backfill below finds a request's matching observations once, in a
-- materialized CTE, and the UPDATE looks its row up there.  Requests with no match
-- have no row, and take the fallback.
--
-- KNOWN APPROXIMATION, ACCEPTED (sc-9766): matching ignores the target, because an
-- observation's configuration coordinates are computed in Scala (proper motion to
-- the CfP reference time, asterism centre or explicit base, ephemerides, regions)
-- and cannot be reproduced here.  So:
--
--   * a filter union may include filters used only by other targets' observations
--     (permissive: an approved request may cover a little more than was asked), and
--   * a request holds one GNIRS camera / GHOST resolution mode.  Where its matching
--     observations disagree the most common value is kept: observations using
--     another become Unapproved, and a request may get a value that only another
--     target's observations use.
--
-- Splitting such requests into one row per value was tried and rejected: without
-- target matching it mints approved rows that cover nothing.  Production had no
-- request whose matching observations disagree (checked 2026-11-03), and the system
-- is still in commissioning, so occasional Unapproved observations or unused
-- approved configurations were judged acceptable.  A faithful backfill would have
-- to run in Scala, where target coverage can be computed.

-- Imaging filters: the union of the stored filters (GMOS only) and those of the
-- matching observations.  Under sc-8036 any filter was approved, so widening to
-- every filter in use keeps each existing observation covered rather than turning
-- it Unapproved.
WITH matched AS MATERIALIZED (
  SELECT cr.c_configuration_request_id AS id,
         array_agg(f) FILTER (WHERE f IS NOT NULL) AS filters
    FROM t_configuration_request cr
    CROSS JOIN LATERAL pg_temp.matching_observations(cr) AS o(oid)
    JOIN v_gmos_north_imaging m ON m.c_observation_id = o.oid
    LEFT JOIN LATERAL unnest(m.c_filters) AS f ON true
   WHERE cr.c_observing_mode_type = 'gmos_north_imaging'
   GROUP BY 1
)
UPDATE t_configuration_request cr
   SET c_gmos_north_imaging_filters = pg_temp.canonical_tags(
         cr.c_gmos_north_imaging_filters ||
         COALESCE((SELECT filters FROM matched WHERE id = cr.c_configuration_request_id), '{}'::_d_tag))
 WHERE cr.c_observing_mode_type = 'gmos_north_imaging';

WITH matched AS MATERIALIZED (
  SELECT cr.c_configuration_request_id AS id,
         array_agg(f) FILTER (WHERE f IS NOT NULL) AS filters
    FROM t_configuration_request cr
    CROSS JOIN LATERAL pg_temp.matching_observations(cr) AS o(oid)
    JOIN v_gmos_south_imaging m ON m.c_observation_id = o.oid
    LEFT JOIN LATERAL unnest(m.c_filters) AS f ON true
   WHERE cr.c_observing_mode_type = 'gmos_south_imaging'
   GROUP BY 1
)
UPDATE t_configuration_request cr
   SET c_gmos_south_imaging_filters = pg_temp.canonical_tags(
         cr.c_gmos_south_imaging_filters ||
         COALESCE((SELECT filters FROM matched WHERE id = cr.c_configuration_request_id), '{}'::_d_tag))
 WHERE cr.c_observing_mode_type = 'gmos_south_imaging';

WITH matched AS MATERIALIZED (
  SELECT cr.c_configuration_request_id AS id,
         array_agg(f) FILTER (WHERE f IS NOT NULL) AS filters
    FROM t_configuration_request cr
    CROSS JOIN LATERAL pg_temp.matching_observations(cr) AS o(oid)
    JOIN v_flamingos_2_imaging m ON m.c_observation_id = o.oid
    LEFT JOIN LATERAL unnest(m.c_filters) AS f ON true
   WHERE cr.c_observing_mode_type = 'flamingos_2_imaging'
   GROUP BY 1
)
UPDATE t_configuration_request cr
   SET c_flamingos_2_imaging_filters = pg_temp.canonical_tags(
         COALESCE((SELECT filters FROM matched WHERE id = cr.c_configuration_request_id), '{}'::_d_tag))
 WHERE cr.c_observing_mode_type = 'flamingos_2_imaging';

-- GNIRS camera and filters: the most common camera among the matching
-- observations, and all of their filters.  With no match at all nothing is covered
-- either way, so any camera will do.
WITH per_obs AS MATERIALIZED (
  SELECT cr.c_configuration_request_id AS id, m.c_camera AS camera, m.c_filters AS filters
    FROM t_configuration_request cr
    CROSS JOIN LATERAL pg_temp.matching_observations(cr) AS o(oid)
    JOIN v_gnirs_imaging m ON m.c_observation_id = o.oid
   WHERE cr.c_observing_mode_type = 'gnirs_imaging'
), matched AS MATERIALIZED (
  SELECT p.id,
         mode() WITHIN GROUP (ORDER BY p.camera) AS camera,
         (SELECT array_agg(f) FROM per_obs q, unnest(q.filters) AS f WHERE q.id = p.id) AS filters
    FROM per_obs p
   GROUP BY p.id
)
UPDATE t_configuration_request cr
   SET c_gnirs_imaging_camera = COALESCE(
         (SELECT camera FROM matched WHERE id = cr.c_configuration_request_id), 'ShortBlue'),
       c_gnirs_imaging_filters = pg_temp.canonical_tags(
         (SELECT filters FROM matched WHERE id = cr.c_configuration_request_id))
 WHERE cr.c_observing_mode_type = 'gnirs_imaging';

-- GHOST: the most common resolution mode among the matching observations, or
-- 'standard' when nothing matches.
WITH matched AS MATERIALIZED (
  SELECT cr.c_configuration_request_id AS id,
         mode() WITHIN GROUP (ORDER BY m.c_resolution_mode) AS resolution_mode
    FROM t_configuration_request cr
    CROSS JOIN LATERAL pg_temp.matching_observations(cr) AS o(oid)
    JOIN v_ghost_ifu m ON m.c_observation_id = o.oid
   WHERE cr.c_observing_mode_type = 'ghost_ifu'
   GROUP BY 1
)
UPDATE t_configuration_request cr
   SET c_ghost_ifu_resolution_mode = COALESCE(
         (SELECT resolution_mode FROM matched WHERE id = cr.c_configuration_request_id), 'standard')
 WHERE cr.c_observing_mode_type = 'ghost_ifu';

DROP FUNCTION pg_temp.matching_observations(t_configuration_request);
DROP FUNCTION pg_temp.canonical_tags(_d_tag);

ALTER TABLE t_configuration_request
  ADD CONSTRAINT t_configuration_request_flamingos_2_imaging_filters_check
    CHECK (c_flamingos_2_imaging_filters IS NOT NULL = (c_observing_mode_type = 'flamingos_2_imaging'::e_observing_mode_type)),
  ADD CONSTRAINT t_configuration_request_gnirs_imaging_camera_check
    CHECK (c_gnirs_imaging_camera IS NOT NULL = (c_observing_mode_type = 'gnirs_imaging'::e_observing_mode_type)),
  ADD CONSTRAINT t_configuration_request_gnirs_imaging_filters_check
    CHECK (c_gnirs_imaging_filters IS NOT NULL = (c_observing_mode_type = 'gnirs_imaging'::e_observing_mode_type)),
  ADD CONSTRAINT t_configuration_request_ghost_ifu_resolution_mode_check
    CHECK (c_ghost_ifu_resolution_mode IS NOT NULL = (c_observing_mode_type = 'ghost_ifu'::e_observing_mode_type));

-- Array elements cannot be checked by a foreign key.  Body copied from V1031, plus
-- the two new arrays.
CREATE OR REPLACE FUNCTION t_configuration_request_array_check()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF (NEW.c_gmos_north_imaging_filters IS NOT NULL) THEN
    IF NOT (ARRAY(SELECT c_tag FROM t_gmos_north_filter) @> NEW.c_gmos_north_imaging_filters) THEN
      RAISE EXCEPTION 'Invalid GMOS north filter element in array column.';
    END IF;
  END IF;
  IF (NEW.c_gmos_south_imaging_filters IS NOT NULL) THEN
    IF NOT (ARRAY(SELECT c_tag FROM t_gmos_south_filter) @> NEW.c_gmos_south_imaging_filters) THEN
      RAISE EXCEPTION 'Invalid GMOS south filter element in array column.';
    END IF;
  END IF;
  IF (NEW.c_flamingos_2_imaging_filters IS NOT NULL) THEN
    IF NOT (ARRAY(SELECT c_tag FROM t_f2_filter) @> NEW.c_flamingos_2_imaging_filters) THEN
      RAISE EXCEPTION 'Invalid Flamingos-2 filter element in array column.';
    END IF;
  END IF;
  IF (NEW.c_gnirs_imaging_filters IS NOT NULL) THEN
    IF NOT (ARRAY(SELECT c_tag FROM t_gnirs_filter) @> NEW.c_gnirs_imaging_filters) THEN
      RAISE EXCEPTION 'Invalid GNIRS filter element in array column.';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Every new item constrains approval, so all of them join the key, together with
-- the GMOS imaging filters (left out until now under sc-8036).  The widening above
-- cannot make two rows collide: the old key ignored filters, so there was at most
-- one GMOS imaging request for each value of the rest of the key.
--
-- With them the key would have 35 columns, over Postgres's limit of 32 for an index.
-- The mode-specific parameters are therefore packed into a single composite value.
-- Btree compares composite values field by field with NULLs equal (unlike `=` on
-- rows), which is exactly the NULLS NOT DISTINCT behaviour of the columns it
-- replaces, and what `SelectRequest` matches field by field with IS NOT DISTINCT
-- FROM.  A unique index (unlike a constraint) may hold an expression, and
-- `ON CONFLICT DO NOTHING` without a conflict target honours any unique index.
-- A new mode parameter goes in this type (and in the index expression below).
CREATE TYPE configuration_request_mode_key AS (
  flamingos_2_longslit_disperser d_tag,
  flamingos_2_imaging_filters    _d_tag,
  gmos_north_longslit_grating    d_tag,
  gmos_south_longslit_grating    d_tag,
  gmos_north_ifu_grating         d_tag,
  gmos_north_ifu_fpu             d_tag,
  gmos_south_ifu_grating         d_tag,
  gmos_south_ifu_fpu             d_tag,
  gmos_north_imaging_filters     _d_tag,
  gmos_south_imaging_filters     _d_tag,
  gnirs_longslit_grating         d_tag,
  gnirs_longslit_camera          d_tag,
  gnirs_longslit_prism           d_tag,
  gnirs_ifu_grating              d_tag,
  gnirs_ifu_fpu                  e_gnirs_fpu_ifu,
  gnirs_imaging_camera           d_tag,
  gnirs_imaging_filters          _d_tag,
  ghost_ifu_resolution_mode      d_tag,
  visitor_radius                 d_angle_µas
);

ALTER TABLE t_configuration_request
  DROP CONSTRAINT t_configuration_request_unique;

CREATE UNIQUE INDEX t_configuration_request_unique ON t_configuration_request (
  c_program_id,
  c_cloud_extinction,
  c_image_quality,
  c_sky_background,
  c_water_vapor,
  c_reference_ra,
  c_reference_dec,
  c_region_ra_arc_type,
  c_region_ra_arc_start,
  c_region_ra_arc_end,
  c_region_dec_arc_type,
  c_region_dec_arc_start,
  c_region_dec_arc_end,
  c_observing_mode_type,
  c_altair_mode,
  c_scheduling_mode,
  (ROW(
    c_flamingos_2_longslit_disperser,
    c_flamingos_2_imaging_filters,
    c_gmos_north_longslit_grating,
    c_gmos_south_longslit_grating,
    c_gmos_north_ifu_grating,
    c_gmos_north_ifu_fpu,
    c_gmos_south_ifu_grating,
    c_gmos_south_ifu_fpu,
    c_gmos_north_imaging_filters,
    c_gmos_south_imaging_filters,
    c_gnirs_longslit_grating,
    c_gnirs_longslit_camera,
    c_gnirs_longslit_prism,
    c_gnirs_ifu_grating,
    c_gnirs_ifu_fpu,
    c_gnirs_imaging_camera,
    c_gnirs_imaging_filters,
    c_ghost_ifu_resolution_mode,
    c_visitor_radius
  )::configuration_request_mode_key)
) NULLS NOT DISTINCT;

-- v_configuration_request selects *, so it is recreated to pick up the new
-- columns.  Body copied from V1328, plus the three new discriminating ids.
DROP VIEW v_configuration_request;

CREATE VIEW v_configuration_request AS
  SELECT
    *,
    CASE WHEN cr.c_reference_ra IS NOT NULL THEN cr.c_configuration_request_id END AS c_reference_id,
    CASE WHEN cr.c_region_ra_arc_type IS NOT NULL THEN cr.c_configuration_request_id END AS c_region_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_imaging' THEN cr.c_configuration_request_id END AS c_flamingos_2_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_long_slit' THEN cr.c_configuration_request_id END AS c_flamingos_2_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_mos' THEN cr.c_configuration_request_id END AS c_flamingos_2_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'ghost_ifu' THEN cr.c_configuration_request_id END AS c_ghost_ifu_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_imaging' THEN cr.c_configuration_request_id END AS c_gmos_north_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_imaging' THEN cr.c_configuration_request_id END AS c_gmos_south_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_north_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_south_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_mos' THEN cr.c_configuration_request_id END AS c_gmos_north_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_mos' THEN cr.c_configuration_request_id END AS c_gmos_south_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gnirs_imaging' THEN cr.c_configuration_request_id END AS c_gnirs_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'igrins_2_long_slit' THEN cr.c_configuration_request_id END AS c_igrins_2_longslit_id,
    CASE WHEN cr.c_gmos_north_ifu_grating IS NOT NULL AND cr.c_gmos_north_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_north_ifu_id,
    CASE WHEN cr.c_gmos_south_ifu_grating IS NOT NULL AND cr.c_gmos_south_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_south_ifu_id,
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_gnirs_ifu_grating IS NOT NULL AND cr.c_gnirs_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_ifu_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;
