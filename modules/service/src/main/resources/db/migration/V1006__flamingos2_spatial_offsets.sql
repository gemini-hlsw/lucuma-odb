-- Add spatial offsets to Flamingos2 long slit mode table
ALTER TABLE t_flamingos_2_long_slit
ADD COLUMN c_spatial_offsets text NULL;

ALTER TABLE t_flamingos_2_long_slit
ADD CONSTRAINT flamingos2_spatial_offsets_format CHECK (
  c_spatial_offsets IS NULL OR 
  c_spatial_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?){7}$'
);

DROP VIEW IF EXISTS v_flamingos_2_long_slit;
CREATE OR REPLACE VIEW v_flamingos_2_long_slit AS
SELECT
  m.*
FROM
  t_flamingos_2_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id;
