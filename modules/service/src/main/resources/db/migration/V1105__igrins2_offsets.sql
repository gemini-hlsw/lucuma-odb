-- spatial offsets for IGRINS-2 Long Slit
ALTER TABLE t_igrins_2_long_slit
  ADD COLUMN c_spatial_offsets text NULL;

ALTER TABLE t_igrins_2_long_slit
  ADD CONSTRAINT check_igrins2_offsets_format
  CHECK (c_spatial_offsets ~ '^(-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*)?$');

-- View of igrins2 long slit that computes default spatial offsets
CREATE VIEW v_igrins_2_long_slit AS
SELECT
  m.*,
  CASE COALESCE(m.c_offset_mode, 'nod_along_slit')
    WHEN 'nod_along_slit' THEN '0.000000,-1.250000,0.000000,1.250000,0.000000,1.250000,0.000000,-1.250000'
    WHEN 'nod_to_sky'     THEN '0.000000,0.000000,10.000000,10.000000,0.000000,0.000000'
  END AS c_default_spatial_offsets
FROM t_igrins_2_long_slit m;

-- Trigger: NodAlongSlit offsets must have p = 0
CREATE OR REPLACE FUNCTION check_igrins2_nod_along_slit_offsets()
RETURNS TRIGGER AS $$
DECLARE
  vals text[];
  i integer;
BEGIN
  IF NEW.c_spatial_offsets IS NOT NULL
     AND COALESCE(NEW.c_offset_mode, 'nod_along_slit') = 'nod_along_slit' THEN
    vals := string_to_array(NEW.c_spatial_offsets, ',');
    FOR i IN 1..array_length(vals, 1) BY 2 LOOP
      IF vals[i]::numeric <> 0 THEN
        RAISE EXCEPTION 'IGRINS-2 NodAlongSlit offsets must have p = 0';
      END IF;
    END LOOP;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_igrins2_nod_along_slit_offsets_trigger
  BEFORE INSERT OR UPDATE ON t_igrins_2_long_slit
  FOR EACH ROW
  EXECUTE FUNCTION check_igrins2_nod_along_slit_offsets();
