-- hold on to the initial disperser, filter, fpu regardless of subsequent changes
ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_initial_disperser d_tag NOT NULL          REFERENCES t_f2_disperser(c_tag);

ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_initial_filter    d_tag NULL DEFAULT NULL REFERENCES t_f2_filter(c_tag);

ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_initial_fpu       d_tag NOT NULL          REFERENCES t_f2_fpu(c_tag);

-- recreate flamingos_2_long_slit view of observing modes
CREATE OR REPLACE VIEW v_flamingos_2_long_slit AS
SELECT
  m.*
FROM
  t_flamingos_2_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id;

