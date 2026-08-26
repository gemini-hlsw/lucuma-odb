-- Archive Duplication Search staleness.
--
-- The stored snapshot is frozen evidence of a search that ran and no changes
-- are taken into account.
--
-- The Configuration the search actually ran against, so staleness compares the
-- present against what was asked, not against a reconstruction.
--
-- No stored attempt time: a successful attempt's time is c_last_checked_at and
-- a failed one's is c_error_at, so the view derives it as the coalesce of the
-- two.  Clients need it to avoid re-firing a refresh that just failed.
ALTER TABLE t_archive_duplication
  ADD COLUMN c_searched_configuration jsonb NULL,
  ADD COLUMN c_error_at               timestamp NULL,
  -- One-directional because rows stored before this migration may carry an
  -- error with no timestamp; a successful store clears both together.
  ADD CONSTRAINT archive_duplication_error_at CHECK (c_error_at IS NULL OR c_error IS NOT NULL);

COMMENT ON COLUMN t_archive_duplication.c_searched_configuration IS
  'The Configuration the snapshot''s search ran against (configuration-request JSON encoding).  Frozen provenance: staleness is derived by comparing it with the observation''s current configuration.';
COMMENT ON COLUMN t_archive_duplication.c_error_at IS
  'When the most recent failed search ran; cleared by a successful one, whose time is c_last_checked_at.';

-- Materialized by the obscalc worker alongside the workflow: true when the
-- stored snapshot no longer applies to the observation as it now stands.
ALTER TABLE t_obscalc
  ADD COLUMN c_archive_stale boolean NOT NULL DEFAULT false;

COMMENT ON COLUMN t_obscalc.c_archive_stale IS
  'Whether the Archive Duplication snapshot''s searched configuration no longer subsumes the observation''s current one.  Computed by the obscalc worker; false when there is nothing stored to go stale.';

-- Writing a snapshot must schedule a staleness recalculation, both so a fresh
-- search reads not-stale once the worker catches up and so a first search is
-- evaluated at all.
CREATE OR REPLACE FUNCTION archive_duplication_obscalc_invalidate()
RETURNS trigger AS $$
BEGIN
  CALL invalidate_obscalc(COALESCE(NEW.c_observation_id, OLD.c_observation_id));
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER archive_duplication_invalidate_obscalc_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_archive_duplication
  FOR EACH ROW
  EXECUTE FUNCTION archive_duplication_obscalc_invalidate();

-- The state an observation reads is now derived: no observing mode means the
-- search cannot be asked today, whatever was stored.  A stored NOT_APPLICABLE
-- with a mode now present still reads NOT_APPLICABLE; c_stale is what flags it
-- for a re-check.
--
-- Dropped and recreated because c_last_attempted_at sits beside
-- c_last_checked_at, and REPLACE cannot insert a column mid-list.
DROP VIEW v_archive_duplication;

CREATE VIEW v_archive_duplication AS
  SELECT
    o.c_observation_id,
    CASE WHEN o.c_observing_mode_type IS NULL
         THEN 'not_applicable'::e_archive_duplication_state
         ELSE COALESCE(d.c_state, 'not_checked'::e_archive_duplication_state)
    END                                                         AS c_state,
    COALESCE(m.c_match_count, 0)                                AS c_match_count,
    COALESCE(d.c_saturated, FALSE)                              AS c_saturated,
    d.c_last_checked_at,
    COALESCE(d.c_error_at, d.c_last_checked_at)                 AS c_last_attempted_at,
    d.c_error,
    d.c_search_ra,
    d.c_search_dec,
    d.c_search_target,
    d.c_search_radius,
    CASE WHEN d.c_search_ra     IS NOT NULL THEN o.c_observation_id END AS c_search_center_id,
    CASE WHEN d.c_search_radius IS NOT NULL THEN o.c_observation_id END AS c_search_radius_id,
    COALESCE(d.c_query_urls, ARRAY[]::text[])                   AS c_query_urls,
    COALESCE(oc.c_archive_stale, FALSE)                         AS c_stale
  FROM t_observation o
  LEFT JOIN t_archive_duplication d ON d.c_observation_id = o.c_observation_id
  LEFT JOIN t_obscalc oc ON oc.c_observation_id = o.c_observation_id
  LEFT JOIN (
    SELECT c_observation_id, COUNT(*)::int4 AS c_match_count
    FROM t_archive_match
    GROUP BY c_observation_id
  ) m ON m.c_observation_id = o.c_observation_id;

-- Existing snapshots were stored before c_searched_configuration existed, so
-- have their staleness evaluated now rather than on the next unrelated edit.
DO $$
DECLARE
  obs d_observation_id;
BEGIN
  FOR obs IN SELECT c_observation_id FROM t_archive_duplication LOOP
    CALL invalidate_obscalc(obs);
  END LOOP;
END;
$$;
