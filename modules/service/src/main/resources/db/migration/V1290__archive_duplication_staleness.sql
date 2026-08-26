-- Archive Duplication Search staleness.
--
-- Existing snapshots are dropped rather than migrated: the feature is not yet
-- in active use.
DELETE FROM t_archive_duplication;

ALTER TABLE t_archive_duplication
  ADD COLUMN c_error_at timestamp NULL,
  -- A failed attempt sets both; a successful store clears both.
  ADD CONSTRAINT archive_duplication_error_at CHECK ((c_error_at IS NULL) = (c_error IS NULL));

COMMENT ON COLUMN t_archive_duplication.c_error_at IS
  'When the most recent failed search ran; cleared by a successful one, whose time is c_last_checked_at.';

-- Materialized by the obscalc worker.
ALTER TABLE t_obscalc
  ADD COLUMN c_archive_stale boolean NOT NULL DEFAULT false;

COMMENT ON COLUMN t_obscalc.c_archive_stale IS
  'Whether the Archive Duplication snapshot''s stored GOA queries differ from the ones the search policy would run today.';

-- Snapshot writes schedule a staleness recalculation; the other obscalc
-- triggers only see observation edits.
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
-- stored: the search cannot be asked for it today.  The stored error is hidden
-- with it, since `error` is documented as accompanying the ERROR state;
-- c_last_attempted_at still reports the failed attempt.
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
    CASE WHEN o.c_observing_mode_type IS NULL
         THEN NULL
         ELSE d.c_error
    END                                                         AS c_error,
    d.c_search_ra,
    d.c_search_dec,
    d.c_search_target,
    d.c_search_radius,
    CASE WHEN d.c_search_ra     IS NOT NULL THEN o.c_observation_id END AS c_search_center_id,
    CASE WHEN d.c_search_radius IS NOT NULL THEN o.c_observation_id END AS c_search_radius_id,
    COALESCE(d.c_query_urls, ARRAY[]::text[])                   AS c_query_urls,
    (COALESCE(oc.c_archive_stale, FALSE)
      AND p.c_proposal_status = 'not_submitted')                AS c_stale
  FROM t_observation o
  JOIN t_program p ON p.c_program_id = o.c_program_id
  LEFT JOIN t_archive_duplication d ON d.c_observation_id = o.c_observation_id
  LEFT JOIN t_obscalc oc ON oc.c_observation_id = o.c_observation_id
  LEFT JOIN (
    SELECT c_observation_id, COUNT(*)::int4 AS c_match_count
    FROM t_archive_match
    GROUP BY c_observation_id
  ) m ON m.c_observation_id = o.c_observation_id;
