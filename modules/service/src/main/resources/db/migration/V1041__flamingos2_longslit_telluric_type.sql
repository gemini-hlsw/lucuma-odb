
ALTER TABLE t_flamingos_2_long_slit
ADD COLUMN c_telluric_type jsonb NOT NULL DEFAULT '{"Hot":{}}'::jsonb;

-- Recreate the view to include the new column
CREATE OR REPLACE VIEW v_flamingos_2_long_slit AS
SELECT
  m.*
FROM
  t_flamingos_2_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id;
