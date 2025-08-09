-- Rename F2 spatial offsets
ALTER TABLE t_flamingos_2_long_slit RENAME COLUMN c_spatial_offsets TO c_offsets;

ALTER TABLE t_flamingos_2_long_slit DROP CONSTRAINT flamingos2_spatial_offsets_format;
ALTER TABLE t_flamingos_2_long_slit ADD CONSTRAINT flamingos2_offsets_format CHECK (c_offsets IS NULL OR c_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?){7}$');

-- recreate the views
DROP VIEW IF EXISTS v_flamingos_2_long_slit;
CREATE OR REPLACE VIEW v_flamingos_2_long_slit AS
SELECT
  m.*
FROM
  t_flamingos_2_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id;
