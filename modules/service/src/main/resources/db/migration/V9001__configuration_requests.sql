

CREATE DOMAIN d_configuration_request_id AS VARCHAR; 
CREATE SEQUENCE s_configuration_request_id START WITH 256;

CREATE TYPE e_configuration_request_status
 AS ENUM('requested', 'approved', 'denied');

CREATE TABLE t_configuration_request (

  -- identifying information
  c_configuration_request_id d_configuration_request_id NOT NULL PRIMARY KEY DEFAULT 'x-' || to_hex(nextval('s_configuration_request_id')),
  c_program_id d_program_id NOT NULL REFERENCES t_program (c_program_id),

  -- status
  c_status e_configuration_request_status NOT NULL DEFAULT 'requested'::e_configuration_request_status,

  -- observing conditions 
  c_cloud_extinction d_tag NOT NULL REFERENCES t_cloud_extinction(c_tag),
  c_image_quality d_tag NOT NULL REFERENCES t_image_quality(c_tag),
  c_sky_background d_tag NOT NULL REFERENCES t_sky_background(c_tag),
  c_water_vapor d_tag NOT NULL REFERENCES t_water_vapor(c_tag),

  -- reference coordinates 
  c_reference_ra d_angle_µas NOT NULL,
  c_reference_dec d_angle_µas NOT NULL,

  -- observing mode 
  c_observing_mode_type e_observing_mode_type NOT NULL,

  -- gmos-n longslit
  c_gmos_north_longslit_grating d_tag NULL REFERENCES t_gmos_north_disperser(c_tag) 
    CHECK (c_gmos_north_longslit_grating IS NOT NULL = (c_observing_mode_type = 'gmos_north_long_slit'::e_observing_mode_type)),

  -- gmos-s longslit
  c_gmos_south_longslit_grating d_tag NULL REFERENCES t_gmos_south_disperser(c_tag) 
    CHECK (c_gmos_south_longslit_grating IS NOT NULL = (c_observing_mode_type = 'gmos_south_long_slit'::e_observing_mode_type))
 
)