-- Create an obscalc view to get synthetic ids for nullable embedded objects.

CREATE VIEW v_obscalc AS
  SELECT o.*,
  CASE WHEN o.c_full_setup_time IS NOT NULL THEN o.c_observation_id END AS c_exe_digest_id,
  CASE WHEN o.c_acq_obs_class   IS NOT NULL THEN o.c_observation_id END AS c_acq_digest_id,
  CASE WHEN o.c_sci_obs_class   IS NOT NULL THEN o.c_observation_id END AS c_sci_digest_id
  FROM t_obscalc o;