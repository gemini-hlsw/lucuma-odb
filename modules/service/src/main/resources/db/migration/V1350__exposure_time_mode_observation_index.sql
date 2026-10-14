-- The only index leading with c_observation_id is the partial unique
-- exposure_time_mode_role_constraint, which covers just the requirement and
-- acquisition rows.  The planner can use it only when it can prove the query's
-- role is one of those two, so a lookup by observation alone (check_etm_consistent,
-- clone_exposure_time_modes), by the science role, or by a role bound as a
-- parameter in a generic plan (the per-mode service selects) scans the whole
-- table.  Under load the table grows with every new observation, and each scan
-- grows with it.
CREATE INDEX i_exposure_time_mode_observation_role
  ON t_exposure_time_mode (c_observation_id, c_role);
