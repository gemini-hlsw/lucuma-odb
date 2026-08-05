-- Archive Duplication Search provenance: the GOA query URL(s) a snapshot was
-- gathered from.
--
-- The search fans an observation out into one GOA query per instrument in its
-- equivalence group, so the snapshot stores a URL per query.
--
-- The host is GOA's canonical base (archive.gemini.edu)
-- re-fetching a stored URL will not reproduce the snapshot.
ALTER TABLE t_archive_duplication
  ADD COLUMN c_query_urls text[] NOT NULL DEFAULT '{}';

COMMENT ON COLUMN t_archive_duplication.c_query_urls IS
  'The GOA query URLs the snapshot was gathered from, one per fan-out query.  Advisory provenance: the search policy and the archive contents both change over time, so these URLs cannot reproduce the snapshot.';

-- Expose the URLs through the view, defaulting to an empty array for an
-- observation that has never been searche
CREATE OR REPLACE VIEW v_archive_duplication AS
  SELECT
    o.c_observation_id,
    COALESCE(d.c_state, 'not_checked'::e_archive_duplication_state) AS c_state,
    COALESCE(m.c_match_count, 0)                                AS c_match_count,
    COALESCE(d.c_saturated, FALSE)                              AS c_saturated,
    d.c_last_checked_at,
    d.c_error,
    d.c_search_ra,
    d.c_search_dec,
    d.c_search_target,
    d.c_search_radius,
    CASE WHEN d.c_search_ra     IS NOT NULL THEN o.c_observation_id END AS c_search_center_id,
    CASE WHEN d.c_search_radius IS NOT NULL THEN o.c_observation_id END AS c_search_radius_id,
    COALESCE(d.c_query_urls, ARRAY[]::text[])                   AS c_query_urls
  FROM t_observation o
  LEFT JOIN t_archive_duplication d ON d.c_observation_id = o.c_observation_id
  LEFT JOIN (
    SELECT c_observation_id, COUNT(*)::int4 AS c_match_count
    FROM t_archive_match
    GROUP BY c_observation_id
  ) m ON m.c_observation_id = o.c_observation_id;
