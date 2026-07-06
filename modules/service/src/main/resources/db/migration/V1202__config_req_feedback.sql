-- Configuration requests can have staff feedback, and we track creation and
-- modification times.
ALTER TABLE t_configuration_request
  ADD COLUMN c_feedback text CHECK (c_feedback IS NULL OR length(c_feedback) > 0),
  ADD COLUMN c_created_at timestamp NOT NULL DEFAULT now(),
  ADD COLUMN c_updated_at timestamp NOT NULL DEFAULT now();

-- Keep c_updated_at current (ch_update_updated_at is defined in V0240).
CREATE TRIGGER ch_update_updated_at_trigger
  BEFORE UPDATE ON t_configuration_request
  FOR EACH ROW
  EXECUTE PROCEDURE ch_update_updated_at();

-- Re-create the view to pick up the new columns.
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
    CASE WHEN cr.c_observing_mode_type = 'igrins_2_long_slit' THEN cr.c_configuration_request_id END AS c_igrins_2_longslit_id,
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_gnirs_ifu_grating IS NOT NULL AND cr.c_gnirs_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_ifu_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;
