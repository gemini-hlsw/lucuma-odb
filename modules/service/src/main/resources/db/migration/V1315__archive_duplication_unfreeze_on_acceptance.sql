-- The Archive Duplication Search snapshot is read-only under the Submission
-- Freeze (proposal submitted or not accepted; acceptance lifts it) and the
-- Completion Freeze (observation complete).  See CONTEXT.md.

-- Completion as the PI declared it or as execution reached it.
CREATE OR REPLACE FUNCTION observation_completed(
  declared e_execution_state,
  workflow e_workflow_state
) RETURNS boolean AS $$
  SELECT COALESCE(declared = 'declared_complete', FALSE)
      OR COALESCE(workflow = 'completed', FALSE);
$$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE VIEW v_archive_duplication AS
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
      AND p.c_proposal_status IN ('not_submitted', 'accepted')
      AND NOT observation_completed(o.c_declared_state, oc.c_workflow_state)) AS c_stale
  FROM t_observation o
  JOIN t_program p ON p.c_program_id = o.c_program_id
  LEFT JOIN t_archive_duplication d ON d.c_observation_id = o.c_observation_id
  LEFT JOIN t_obscalc oc ON oc.c_observation_id = o.c_observation_id
  LEFT JOIN (
    SELECT c_observation_id, COUNT(*)::int4 AS c_match_count
    FROM t_archive_match
    GROUP BY c_observation_id
  ) m ON m.c_observation_id = o.c_observation_id;
