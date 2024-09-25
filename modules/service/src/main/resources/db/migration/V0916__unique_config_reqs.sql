
-- Configuration requests should be unique per program, conditions, base position, and config
ALTER TABLE t_configuration_request
  ADD UNIQUE NULLS NOT DISTINCT (
    c_program_id,
    c_cloud_extinction,
    c_image_quality,
    c_sky_background,
    c_water_vapor,
    c_reference_ra,
    c_reference_dec,
    c_observing_mode_type,
    c_gmos_north_longslit_grating,
    c_gmos_south_longslit_grating
  );


