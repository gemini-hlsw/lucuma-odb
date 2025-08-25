

-- Add fields for f2 longslit and gmos n/s imaging.
ALTER TABLE t_configuration_request
  ADD c_flamingos_2_longslit_disperser d_tag NULL REFERENCES t_f2_disperser(c_tag)
    CHECK (c_flamingos_2_longslit_disperser IS NOT NULL = (c_observing_mode_type = 'flamingos_2_long_slit'::e_observing_mode_type)),
  ADD c_gmos_north_imaging_filters _d_tag NULL
    CHECK (c_gmos_north_imaging_filters IS NOT NULL = (c_observing_mode_type = 'gmos_north_imaging'::e_observing_mode_type)),
  ADD c_gmos_south_imaging_filters _d_tag NULL
    CHECK (c_gmos_south_imaging_filters IS NOT NULL = (c_observing_mode_type = 'gmos_south_imaging'::e_observing_mode_type))
;

-- We can't validate the filter array elements with an fk or check constraint so we need to do it in a trigger.
CREATE OR REPLACE FUNCTION t_configuration_request_array_check()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF (NEW.c_gmos_north_imaging_filters IS NOT NULL) THEN
    IF NOT (ARRAY(SELECT c_tag FROM t_gmos_north_filter) @> NEW.c_gmos_north_imaging_filters) THEN
      RAISE EXCEPTION 'Invalid GMOS north filter element in array column.';
    END IF;
  END IF;
  IF (NEW.c_gmos_south_imaging_filters IS NOT NULL) THEN
    IF NOT (ARRAY(SELECT c_tag FROM t_gmos_south_filter) @> NEW.c_gmos_south_imaging_filters) THEN
      RAISE EXCEPTION 'Invalid GMOS south filter element in array column.';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER t_configuration_request_array_check_trigger
  AFTER INSERT OR UPDATE ON t_configuration_request
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE t_configuration_request_array_check();

-- Replace view to pick up new stuff.
DROP VIEW v_configuration_request;
CREATE VIEW v_configuration_request AS
  SELECT 
    *,
    CASE WHEN cr.c_reference_ra IS NOT NULL THEN cr.c_configuration_request_id END AS c_reference_id,
    CASE WHEN cr.c_region_ra_arc_type IS NOT NULL THEN cr.c_configuration_request_id END AS c_region_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_long_slit' THEN cr.c_configuration_request_id END AS c_flamingos_2_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_imaging' THEN cr.c_configuration_request_id END AS c_gmos_north_imaging_id,    
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_imaging' THEN cr.c_configuration_request_id END AS c_gmos_south_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_north_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_south_longslit_id,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;