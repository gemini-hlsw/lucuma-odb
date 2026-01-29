

-- This constraint was unnamed before but I looked its name up in the system table.
-- Seems to be deterministic, luckily.
ALTER TABLE t_configuration_request
DROP CONSTRAINT t_configuration_request_c_program_id_c_cloud_extinction_c_i_key;

-- Add F2 disperser to the unique constraint
ALTER TABLE t_configuration_request
  ADD CONSTRAINT t_configuration_request_unique UNIQUE NULLS NOT DISTINCT (
    c_program_id,
    c_cloud_extinction,
    c_image_quality,
    c_sky_background,
    c_water_vapor,
    c_reference_ra,
    c_reference_dec,
    c_observing_mode_type,
    c_gmos_north_longslit_grating,
    c_gmos_south_longslit_grating,
    c_flamingos_2_longslit_disperser
  );
