-- Part 3 of 3 (see V1325, V1326).  Removes resolution from the target, undoing
-- V1258 in full.
--
-- An opportunity target is a placeholder now: it carries a region and never gains
-- coordinates.  When the alert arrives a real target takes its place in the
-- asterism, so there is no in-place resolution to record.
--
-- V1258 separated two facts that c_type had been conflating -- WHICH KIND of
-- target this is, and HOW IT MOVES -- and that separation was worth having only
-- while the two could disagree, which is exactly what resolution made possible.
-- With resolution gone they coincide again: a target moves siderally if it is
-- sidereal, nonsiderally if it is nonsidereal, and not at all if it is a
-- placeholder.  So c_target_tracking_type is not a simplification of c_type any
-- more, it is a copy of it, and it goes too.  lucuma-core drops TargetResolution
-- for the same reason and in the same release.
--
-- The four field constraints go back to asking c_type, in the total form V1258
-- gave them rather than the partial form they had before it -- V1258's drive-by
-- fix there was real and is kept.  The translation is exact: c_type is NOT NULL,
-- so plain = and <> mean what IS [NOT] DISTINCT FROM meant over a nullable
-- tracking type.  An opportunity target is neither 'sidereal' nor 'nonsidereal',
-- so it must leave every coordinate column null -- for its whole life now, rather
-- than only until it resolved.  V1326 converted the rows that would have failed.

-------------------------------------------------------------------------------
-- Dependents come down first.
-------------------------------------------------------------------------------

-- v_target selects three synthetic ids derived from c_resolved_type, and is
-- recreated at the end without them.
DROP VIEW v_target;

-- V1261's trigger names c_resolved_type in its UPDATE OF list, which pins the
-- column: UPDATE OF matches the statement's target columns, so the column cannot
-- be dropped while it is listed.  Recreated below without it.
DROP TRIGGER too_target_track_target_trigger ON t_target;

-- The four constraints reference c_target_tracking_type, so they come off before
-- it does.  They go back below, restated over c_type.
ALTER TABLE t_target
  DROP CONSTRAINT ra_dec_epoch_all_defined,
  DROP CONSTRAINT sidereal_or_all_columns_null,
  DROP CONSTRAINT nonsidereal_all_non_null,
  DROP CONSTRAINT nonsidereal_or_all_columns_null,
  DROP CONSTRAINT resolved_type_only_for_opportunity,
  DROP COLUMN c_target_tracking_type,
  DROP COLUMN c_resolved_type;

-- Nothing refers to it now.  t_target was its only user; lucuma-core's TrackType,
-- which it was named after, remains and is still used for guide star lookup.
DROP TYPE e_target_tracking_type;

-------------------------------------------------------------------------------
-- The target trigger, less the column that no longer exists.
-------------------------------------------------------------------------------

-- A target does not resolve in place any more, so the only things that can change
-- whether an observation holds a placeholder are the target's type and its
-- existence.  Membership is the other trigger's business
-- (too_target_track_asterism_trigger), and it is untouched.
--
-- The function body is unchanged: it only ever called refresh_has_too_target,
-- which V1325 already narrowed to the single remaining flag.
CREATE TRIGGER too_target_track_target_trigger
  AFTER DELETE OR UPDATE OF c_type, c_existence ON t_target
  FOR EACH ROW
  EXECUTE FUNCTION too_target_track_target();

-------------------------------------------------------------------------------
-- Field constraints, restated over c_type.
-------------------------------------------------------------------------------

ALTER TABLE t_target

  -- If it tracks siderally, RA / Dec / epoch must all be present.
  ADD CONSTRAINT ra_dec_epoch_all_defined
  CHECK (
    c_type <> 'sidereal'
    OR num_nulls(c_sid_ra, c_sid_dec, c_sid_epoch) = 0
  ),

  -- If it does not track siderally, every sidereal column must be null --
  -- including the optional ones the check above never mentions.
  ADD CONSTRAINT sidereal_or_all_columns_null
  CHECK (
    c_type = 'sidereal'
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

  -- If it tracks nonsiderally, all three ephemeris columns must be present.
  ADD CONSTRAINT nonsidereal_all_non_null
  CHECK (
    c_type <> 'nonsidereal'
    OR num_nulls(c_nsid_des, c_nsid_key_type, c_nsid_key) = 0
  ),

  -- If it does not, they must all be null.
  ADD CONSTRAINT nonsidereal_or_all_columns_null
  CHECK (
    c_type = 'nonsidereal'
    OR num_nulls(c_nsid_des, c_nsid_key_type, c_nsid_key) = 3
  );

-------------------------------------------------------------------------------
-- View.
-------------------------------------------------------------------------------

-- Body copied from V1258, less the three resolution synthetic ids.  Every other
-- synthetic id is untouched, including c_opportunity_id: the GraphQL one-of
-- discrimination over sidereal / nonsidereal / opportunity is unaffected.
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
  CASE WHEN c_type='opportunity' AND c_opp_ra_arc_type  = 'partial' THEN c_target_id END AS c_opportunity_ra_arc_start_end_synthetic_id

  FROM t_target;
