-- Archive Duplication Search staleness.
--
-- The stored snapshot is frozen evidence of a search that ran and no changes
-- are taken into account.
--
-- We can drop the current set of results, this feature is not yet in active use,
-- and we can simplify the migration this way.
DELETE FROM t_archive_duplication;

ALTER TABLE t_archive_duplication
  ADD COLUMN c_error_at timestamp NULL,
  -- A failed attempt sets both the message and the time; a successful store
  -- clears both together.
  ADD CONSTRAINT archive_duplication_error_at CHECK ((c_error_at IS NULL) = (c_error IS NULL));

COMMENT ON COLUMN t_archive_duplication.c_error_at IS
  'When the most recent failed search ran; cleared by a successful one, whose time is c_last_checked_at.';

-- Materialized by the obscalc worker alongside the workflow: true when the
-- stored snapshot no longer applies to the observation as it now stands.
ALTER TABLE t_obscalc
  ADD COLUMN c_archive_stale boolean NOT NULL DEFAULT false;

COMMENT ON COLUMN t_obscalc.c_archive_stale IS
  'Whether the Archive Duplication snapshot''s stored GOA queries differ from the ones the search policy would run today.';

-- Every snapshot write schedules a staleness recalculation.  The other obscalc
-- triggers only see observation edits, so nothing else would evaluate a new
-- snapshot.
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

-- An observation with no observing mode reads NOT_APPLICABLE, whatever was
-- stored: the search cannot be asked for it today.
--
-- DROP rather than REPLACE: c_last_attempted_at goes in mid-list.
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
