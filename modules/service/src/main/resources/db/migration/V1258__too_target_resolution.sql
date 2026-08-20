-- Target-of-Opportunity targets survive their own resolution.
--
-- Until now, "resolving" a ToO meant destroying it: TargetService rewrote the row
-- from c_type = 'opportunity' to 'sidereal', filled in the coordinates and nulled
-- the region.  The target id and the asterism were unchanged, but every trace that
-- this had ever been a Target of Opportunity was gone -- at exactly the moment the
-- observation became one operationally.
--
-- That is fatal for anything that wants to derive "is this a ToO" from the asterism,
-- and it also throws away the approved search region on first use, so an accepted
-- interrupting ToO could be pointed anywhere in the sky.
--
-- The root cause is that c_type conflates two orthogonal facts: HOW THE TARGET MOVES
-- and WHETHER WE KNOW WHERE IT IS YET.  'opportunity' occupies the same slot as
-- 'sidereal' and 'nonsidereal', so the two facts compete for one field.
--
-- This migration separates them:
--
--   c_type                  -- which kind of target this is.  An opportunity target
--                              stays 'opportunity' for life.  Unchanged for every
--                              existing row.
--   c_resolved_type         -- what an opportunity target turned out to be, once the
--                              alert arrives.  NULL while unresolved; never set on a
--                              target that is not an opportunity.
--   c_target_tracking_type  -- how the target moves, if that is settled yet.  Derived
--                              from the two above.
--
-- Resolution reuses the existing c_sid_* / c_nsid_* columns, so a resolved ToO is
-- stored exactly like the target it resolved to, plus its region.  Nothing about the
-- region columns changes; they simply stop being nulled at resolution.
--
-- c_type stays NOT NULL and keeps 'opportunity' among its values.  That is both more
-- honest -- the target has not ceased to be an opportunity target -- and far cheaper:
-- no enum type swap, no backfill, and every existing predicate in v_target keeps
-- working verbatim.

-------------------------------------------------------------------------------
-- Resolution.
-------------------------------------------------------------------------------

-- How a target moves.  This is deliberately NOT e_target_type: a target of opportunity
-- is a statement about what we know, not about motion, and it cannot resolve to another
-- one.  Giving the two facts different types means nothing downstream has to remember
-- which of them admits all three labels -- and it mirrors lucuma-core's TrackType,
-- which has exactly these two values.
CREATE TYPE e_target_tracking_type AS ENUM ('sidereal', 'nonsidereal');

-- Adding a nullable column with no default is metadata-only: no table rewrite and
-- no trigger events queued.
ALTER TABLE t_target
  ADD COLUMN c_resolved_type e_target_tracking_type NULL;

COMMENT ON COLUMN t_target.c_resolved_type IS
  'What an opportunity target resolved to, or NULL while it is still unresolved. '
  'Never set on a target whose c_type is not ''opportunity''.';

-- The other half of the old constraint -- that a ToO cannot resolve to a ToO -- is now
-- the column's type and needs no check.
ALTER TABLE t_target
  ADD CONSTRAINT resolved_type_only_for_opportunity
  CHECK (c_resolved_type IS NULL OR c_type = 'opportunity');

-------------------------------------------------------------------------------
-- Effective tracking type.
-------------------------------------------------------------------------------

-- "How does this target move, if we know yet."  NULL for an unresolved opportunity
-- target -- a real state, not an encoding artifact.  This is the single definition
-- that decides which of the c_sid_* / c_nsid_* column groups must be populated,
-- shared by the constraints below and by everything reading v_target.
--
-- Generated rather than a function call, so that it is stored, indexable, arrives in
-- v_target through `select *`, and above all cannot disagree with its inputs.  It is
-- the same treatment c_too_activation gets on t_observation.
--
-- The CASE is spelled out rather than written as
--
--   (CASE WHEN c_type = 'opportunity' THEN c_resolved_type ELSE c_type END)
--     ::text::e_target_tracking_type
--
-- because casting between enum types is not immutable, and a generation expression
-- must be.  Naming the literals keeps every branch within one type.  'opportunity'
-- falls to the ELSE, where c_resolved_type answers -- NULL while it is unresolved.
ALTER TABLE t_target
  ADD COLUMN c_target_tracking_type e_target_tracking_type
  GENERATED ALWAYS AS (
    CASE c_type
      WHEN 'sidereal'    THEN 'sidereal'::e_target_tracking_type
      WHEN 'nonsidereal' THEN 'nonsidereal'::e_target_tracking_type
      ELSE                    c_resolved_type
    END
  ) STORED;

COMMENT ON COLUMN t_target.c_target_tracking_type IS
  'How this target moves, or NULL if it is a Target of Opportunity that has not been '
  'resolved yet.  Derived from c_type and c_resolved_type.';

-------------------------------------------------------------------------------
-- Field constraints, restated in terms of the tracking type.
-------------------------------------------------------------------------------

-- Four constraints decide which tracking columns may and must be populated, and all
-- four ask c_type when what they actually mean is "how does this target move".  They
-- come in pairs: one requires the mandatory columns of a group, the other forbids the
-- whole group -- including the optional columns (proper motion, radial velocity,
-- parallax, catalog info) that the first pair never mentions.
--
-- The change to each is mechanical: c_type becomes c_target_tracking_type.  Their
-- meanings are otherwise preserved exactly.  A resolved opportunity target then
-- populates the same columns a plain target of that tracking type would; an
-- unresolved one must leave every one of them null.
--
-- A CHECK may reference a stored generated column, and evaluates it against the value
-- the row is being written with -- verified on PostgreSQL 15.
--
-- IS [NOT] DISTINCT FROM rather than = / <>, because the tracking type is NULL for an
-- unresolved opportunity target.  With a plain comparison the operand would be NULL,
-- the whole check would be NULL, and the constraint would pass vacuously -- letting
-- an unresolved ToO carry half a set of coordinates.

ALTER TABLE t_target

  -- If it tracks siderally, RA / Dec / epoch must all be present.  (From V0140.)
  DROP CONSTRAINT ra_dec_epoch_all_defined,
  ADD CONSTRAINT ra_dec_epoch_all_defined
  CHECK (
    c_target_tracking_type IS DISTINCT FROM 'sidereal'::e_target_tracking_type
    OR num_nulls(c_sid_ra, c_sid_dec, c_sid_epoch) = 0
  ),

  -- If it does not track siderally, every sidereal column must be null.  (From V0070.)
  DROP CONSTRAINT sidereal_or_all_columns_null,
  ADD CONSTRAINT sidereal_or_all_columns_null
  CHECK (
    c_target_tracking_type IS NOT DISTINCT FROM 'sidereal'::e_target_tracking_type
    OR num_nulls(
         c_sid_ra,
         c_sid_dec,
         c_sid_epoch,
         c_sid_pm_ra,
         c_sid_pm_dec,
         c_sid_rv,
         c_sid_parallax,
         c_sid_catalog_name,
         c_sid_catalog_id,
         c_sid_catalog_object_type
       ) = 10
  ),

  -- If it tracks nonsiderally, all three ephemeris columns must be present.  (V0070.)
  DROP CONSTRAINT nonsidereal_all_non_null,
  ADD CONSTRAINT nonsidereal_all_non_null
  CHECK (
    c_target_tracking_type IS DISTINCT FROM 'nonsidereal'::e_target_tracking_type
    OR num_nulls(c_nsid_des, c_nsid_key_type, c_nsid_key) = 0
  ),

  -- If it does not track nonsiderally, they must all be null.  (From V0070.)
  DROP CONSTRAINT nonsidereal_or_all_columns_null,
  ADD CONSTRAINT nonsidereal_or_all_columns_null
  CHECK (
    c_target_tracking_type IS NOT DISTINCT FROM 'nonsidereal'::e_target_tracking_type
    OR num_nulls(c_nsid_des, c_nsid_key_type, c_nsid_key) = 3
  );

-- The region constraints (opportunity_fields, and the per-column gates added in
-- V1007) are unchanged: the region still belongs to opportunity targets and only to
-- them.  It simply outlives resolution now.

-- Drive-by fix, unrelated to ToO resolution but in the constraints being touched.
-- V1007 wrote these as `arc_type = 'full' OR arc_type = 'empty' OR both endpoints
-- present`, which evaluates to NULL -- and therefore passes -- when the arc type
-- itself is NULL.  A non-opportunity target could carry a stray arc endpoint.  State
-- them totally instead.
ALTER TABLE t_target

  DROP CONSTRAINT opportunity_ra,
  ADD CONSTRAINT opportunity_ra
  CHECK (
    CASE c_opp_ra_arc_type
      WHEN 'partial' THEN num_nulls(c_opp_ra_arc_start, c_opp_ra_arc_end) = 0
      ELSE                num_nulls(c_opp_ra_arc_start, c_opp_ra_arc_end) = 2
    END
  ),

  DROP CONSTRAINT opportunity_dec,
  ADD CONSTRAINT opportunity_dec
  CHECK (
    CASE c_opp_dec_arc_type
      WHEN 'partial' THEN num_nulls(c_opp_dec_arc_start, c_opp_dec_arc_end) = 0
      ELSE                num_nulls(c_opp_dec_arc_start, c_opp_dec_arc_end) = 2
    END
  );

-------------------------------------------------------------------------------
-- View.
-------------------------------------------------------------------------------

-- v_target is `select *`, which expands its column list at creation time and so does
-- not pick up c_resolved_type or c_target_tracking_type on its own.  Body otherwise
-- copied verbatim from V1034, plus the resolution synthetic ids.
--
-- Every pre-existing synthetic id is untouched, including c_opportunity_id: a
-- resolved ToO is still, correctly, an opportunity target, so the GraphQL one-of
-- discrimination (exactly one of sidereal / nonsidereal / opportunity is non-null)
-- continues to hold.  The resolution nests inside Opportunity rather than surfacing
-- as a fourth sibling.
DROP VIEW IF EXISTS v_target;
CREATE VIEW v_target AS
  SELECT *,
  CASE WHEN c_sid_catalog_name IS NOT NULL THEN c_target_id END AS c_sid_catalog_info_id,
  CASE WHEN c_sid_pm_ra        IS NOT NULL THEN c_target_id END AS c_sid_pm_id,
  CASE WHEN c_sid_parallax     IS NOT NULL THEN c_target_id END AS c_sid_parallax_id,
  CASE WHEN c_sid_rv           IS NOT NULL THEN c_target_id END AS c_sid_rv_id,
  CASE WHEN c_type='sidereal'              THEN c_target_id END AS c_sidereal_id,
  CASE WHEN c_type='nonsidereal'           THEN c_target_id END AS c_nonsidereal_id,
  CASE WHEN c_type='opportunity'           THEN c_target_id END AS c_opportunity_id,
  CASE WHEN c_type='opportunity' THEN c_target_id END AS c_opportunity_dec_arc_synthetic_id,
  CASE WHEN c_type='opportunity' THEN c_target_id END AS c_opportunity_ra_arc_synthetic_id,
  CASE WHEN c_type='opportunity' AND c_opp_dec_arc_type = 'partial' THEN c_target_id END AS c_opportunity_dec_arc_start_end_synthetic_id,
  CASE WHEN c_type='opportunity' AND c_opp_ra_arc_type  = 'partial' THEN c_target_id END AS c_opportunity_ra_arc_start_end_synthetic_id,

  -- Opportunity.resolution, and the arm of it that is populated.  c_resolved_type is
  -- non-null only for opportunity targets (resolved_type_only_for_opportunity), so
  -- these need no additional c_type test.
  CASE WHEN c_resolved_type IS NOT NULL      THEN c_target_id END AS c_opportunity_resolution_id,
  CASE WHEN c_resolved_type = 'sidereal'     THEN c_target_id END AS c_opportunity_sidereal_id,
  CASE WHEN c_resolved_type = 'nonsidereal'  THEN c_target_id END AS c_opportunity_nonsidereal_id

  FROM t_target;
