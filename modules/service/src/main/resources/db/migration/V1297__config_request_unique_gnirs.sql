-- The uniqueness key has to carry every column that discriminates one request
-- from another.  GNIRS arrived after the key was last rebuilt (V1086), so two
-- GNIRS requests in the same program differing only in grating, camera, prism
-- or aperture leave every mode column in the key NULL and collide under
-- NULLS NOT DISTINCT; `ON CONFLICT DO NOTHING` then returns no row for a
-- genuinely distinct configuration.
--
-- Widening the key can only make rows more distinct, so no existing row can
-- violate the rebuilt constraint.

ALTER TABLE t_configuration_request
  DROP CONSTRAINT t_configuration_request_unique;

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
    c_flamingos_2_longslit_disperser,
    c_gnirs_longslit_grating,
    c_gnirs_longslit_camera,
    c_gnirs_longslit_prism,
    c_gnirs_ifu_grating,
    c_gnirs_ifu_fpu
  );
