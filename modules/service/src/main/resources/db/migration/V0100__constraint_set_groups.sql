CREATE view v_constraint_set_group AS
SELECT
  DISTINCT c_conditions_key,
  c_program_id,
  max(c_observation_id) as c_observation_id -- arbitrary, just pick one
FROM
  t_observation
GROUP BY
  c_conditions_key,
  c_program_id;

