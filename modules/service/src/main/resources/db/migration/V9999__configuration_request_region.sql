
-- Generalize the target information, allowing either reference coords or a region.
ALTER TABLE t_configuration_request
  ALTER c_reference_ra  DROP NOT NULL,
  ALTER c_reference_dec DROP NOT NULL,
  ADD   c_region_ra_arc_type    e_arc_type  NULL,
  ADD   c_region_ra_arc_start   d_angle_µas NULL CHECK (c_region_ra_arc_type = 'partial' OR c_region_ra_arc_start IS NULL),
  ADD   c_region_ra_arc_end     d_angle_µas NULL CHECK (c_region_ra_arc_type = 'partial' OR c_region_ra_arc_end   IS NULL),
  ADD   c_region_dec_arc_type   e_arc_type  NULL,
  ADD   c_region_dec_arc_start  d_angle_µas NULL CHECK (c_region_dec_arc_type = 'partial' OR c_region_dec_arc_start IS NULL),
  ADD   c_region_dec_arc_end    d_angle_µas NULL CHECK (c_region_dec_arc_type = 'partial' OR c_region_dec_arc_end   IS NULL),
  ADD CONSTRAINT t_configuration_request_target_type CHECK (
    (num_nulls(c_reference_ra, c_reference_ra) = 0 AND num_nulls(c_region_ra_arc_type, c_region_dec_arc_type) = 2) OR
    (num_nulls(c_reference_ra, c_reference_ra) = 2 AND num_nulls(c_region_ra_arc_type, c_region_dec_arc_type) = 0)
  )
  ;

-- Replace view to pick up new columns.
DROP VIEW v_configuration_request;
CREATE VIEW v_configuration_request AS
  SELECT 
    *,
    CASE WHEN c_reference_ra IS NOT NULL THEN c_configuration_request_id END AS c_reference_id,
    CASE WHEN c_region_ra_arc_type IS NOT NULL THEN c_configuration_request_id END AS c_region_id,
    CASE WHEN c_observing_mode_type = 'gmos_north_long_slit' THEN c_configuration_request_id END AS c_gmos_north_longslit_id,
    CASE WHEN c_observing_mode_type = 'gmos_south_long_slit' THEN c_configuration_request_id END AS c_gmos_south_longslit_id,
    CASE WHEN c_region_ra_arc_type = 'partial' THEN c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN c_region_dec_arc_type = 'partial' THEN c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request;
