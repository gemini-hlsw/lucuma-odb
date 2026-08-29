-- The uniqueness key must carry exactly the columns that `SelectRequest`
-- (ConfigurationService) matches exactly.  When the key is narrower, two
-- genuinely distinct requests collide, `ON CONFLICT DO NOTHING` yields no row,
-- and the fallback lookup then fails to find the colliding row -- surfacing as
-- "Failed to insert a configuration request ... likely due to an incorrect
-- unique index".
--
-- Missing until now: the region columns (which carry the whole discriminant for
-- opportunity targets, where the reference coordinates are null), the visitor
-- radius, and every GNIRS column.
--
-- The GMOS imaging filter arrays are deliberately excluded: `SelectRequest`
-- matches them by containment (@>), not equality, so imaging canonicalizes onto
-- a superset row on purpose.
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
    c_region_ra_arc_type,
    c_region_ra_arc_start,
    c_region_ra_arc_end,
    c_region_dec_arc_type,
    c_region_dec_arc_start,
    c_region_dec_arc_end,
    c_observing_mode_type,
    c_flamingos_2_longslit_disperser,
    c_gmos_north_longslit_grating,
    c_gmos_south_longslit_grating,
    c_gnirs_longslit_grating,
    c_gnirs_longslit_camera,
    c_gnirs_longslit_prism,
    c_gnirs_ifu_grating,
    c_gnirs_ifu_fpu,
    c_visitor_radius
  );
