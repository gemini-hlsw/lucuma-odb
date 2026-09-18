-- A configuration request is approved for exactly one Altair mode (or for no
-- Altair at all): `Configuration.subsumes` requires the two modes to be equal,
-- so switching the laser on or off, or dropping AO, needs its own approval.

ALTER TABLE t_configuration_request
  ADD COLUMN c_altair_mode e_altair_mode NULL;

-- v_configuration_request selects cr.*, so it must be recreated to pick up the
-- new column. Body copied verbatim from V1297.
DROP VIEW v_configuration_request;

CREATE VIEW v_configuration_request AS
  SELECT
    *,
    CASE WHEN cr.c_reference_ra IS NOT NULL THEN cr.c_configuration_request_id END AS c_reference_id,
    CASE WHEN cr.c_region_ra_arc_type IS NOT NULL THEN cr.c_configuration_request_id END AS c_region_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_long_slit' THEN cr.c_configuration_request_id END AS c_flamingos_2_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_mos' THEN cr.c_configuration_request_id END AS c_flamingos_2_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_imaging' THEN cr.c_configuration_request_id END AS c_gmos_north_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_imaging' THEN cr.c_configuration_request_id END AS c_gmos_south_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_north_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_south_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_mos' THEN cr.c_configuration_request_id END AS c_gmos_north_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_mos' THEN cr.c_configuration_request_id END AS c_gmos_south_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'igrins_2_long_slit' THEN cr.c_configuration_request_id END AS c_igrins_2_longslit_id,
    CASE WHEN cr.c_gmos_north_ifu_grating IS NOT NULL AND cr.c_gmos_north_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_north_ifu_id,
    CASE WHEN cr.c_gmos_south_ifu_grating IS NOT NULL AND cr.c_gmos_south_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_south_ifu_id,
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_gnirs_ifu_grating IS NOT NULL AND cr.c_gnirs_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_ifu_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;

-- The uniqueness key must carry exactly the columns that `SelectRequest`
-- (ConfigurationService) matches exactly, so the Altair mode joins it. NULLS
-- NOT DISTINCT makes "no Altair" a value like any other, the way it already
-- does for every nullable discriminant here.
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
    c_visitor_radius,
    c_gmos_north_ifu_grating,
    c_gmos_north_ifu_fpu,
    c_gmos_south_ifu_grating,
    c_gmos_south_ifu_fpu,
    c_altair_mode
  );

-- An observation's configuration exposes the Altair mode as a plain nullable
-- field, while t_observation.c_altair_mode is already mapped as the non-null
-- mode of the nested Altair object. A grackle ColumnRef is identified by name
-- alone, so the two uses need distinct names (as for c_cass_rotator in V1318).
-- Body copied verbatim from V1318, plus the alias; appending the column at the
-- end is what lets CREATE OR REPLACE stand in for a drop and recreate.
CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
  (o.c_scheduling_mode = 'unconstrained'::e_scheduling_mode) AS c_is_splittable,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,
  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  CASE WHEN o.c_orig_est_setup_count     IS NOT NULL THEN o.c_observation_id END AS c_original_estimate_id,
  CASE WHEN o.c_altair_mode              IS NOT NULL THEN o.c_observation_id END AS c_altair_id,
  -- Alias read as a plain nullable column; c_altair_cass_rotator itself is mapped
  -- as a non-null field of the nested Altair object and a grackle ColumnRef is
  -- identified by name alone, so the two uses need distinct names.
  o.c_altair_cass_rotator AS c_cass_rotator,
  CASE WHEN o.c_science_mode = 'imaging'::d_tag      THEN o.c_observation_id END AS c_imaging_mode_id,
  CASE WHEN o.c_science_mode = 'spectroscopy'::d_tag THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time,
  EXISTS (
    SELECT 1
    FROM t_sequence_materialization m
    WHERE m.c_observation_id = o.c_observation_id
      AND m.c_sequence_type = 'science'::e_sequence_type
  ) AS c_science_sequence_is_materialized,
  EXISTS (
    SELECT 1
    FROM t_sequence_materialization m
    WHERE m.c_observation_id = o.c_observation_id
      AND m.c_sequence_type = 'acquisition'::e_sequence_type
  ) AS c_acquisition_sequence_is_materialized,
  (
    SELECT a.c_target_id
    FROM t_asterism_target a
    WHERE a.c_observation_id = o.c_observation_id
      AND a.c_is_signal_to_noise_target
  ) AS c_signal_to_noise_target_id,
  o.c_altair_mode AS c_configuration_altair_mode
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;
