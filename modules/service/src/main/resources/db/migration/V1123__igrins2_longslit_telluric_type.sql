-- Add telluric type field, defaults to HOT
ALTER TABLE t_igrins_2_long_slit
ADD COLUMN c_telluric_type jsonb NOT NULL DEFAULT '{"tag":"HOT","starTypes":null}'::jsonb;

DROP VIEW v_igrins_2_long_slit;

CREATE VIEW v_igrins_2_long_slit AS
SELECT
  m.*,
  CASE COALESCE(m.c_offset_mode, 'nod_along_slit')
    WHEN 'nod_along_slit' THEN '0.000000,-1.250000,0.000000,1.250000,0.000000,1.250000,0.000000,-1.250000'
    WHEN 'nod_to_sky'     THEN '0.000000,0.000000,10.000000,10.000000,0.000000,0.000000'
  END AS c_default_spatial_offsets
FROM t_igrins_2_long_slit m;
