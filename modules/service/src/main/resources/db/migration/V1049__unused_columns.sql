-- Remove unused planned time summary (pts) columns from t_observation.

DROP VIEW v_observation;

ALTER TABLE t_observation
  DROP COLUMN c_pts_pi,
  DROP COLUMN c_pts_uncharged,
  DROP COLUMN c_pts_execution;

-- Update the observation view (copied from V1044) to remove the pts columns.

CREATE VIEW v_observation AS
  SELECT o.*,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,

  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  CASE WHEN o.c_science_mode = 'imaging'::d_tag      THEN o.c_observation_id END AS c_imaging_mode_id,
  CASE WHEN o.c_science_mode = 'spectroscopy'::d_tag THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;

-- Remove unused planned time summary (pts) columns from t_program and its
-- chron table reflection.

ALTER TABLE t_program
  DROP COLUMN c_pts_pi,
  DROP COLUMN c_pts_uncharged,
  DROP COLUMN c_pts_execution;

ALTER TABLE t_chron_program_update
  DROP COLUMN c_mod_pts_pi,
  DROP COLUMN c_mod_pts_uncharged,
  DROP COLUMN c_mod_pts_execution,
  DROP COLUMN c_new_pts_pi,
  DROP COLUMN c_new_pts_uncharged,
  DROP COLUMN c_new_pts_execution;

-- Update the chron update function (adapted from V0903) to remove the pts
-- references.

CREATE OR REPLACE FUNCTION chron_program_update()
  RETURNS trigger AS $$
DECLARE
  mod_program_id      bool := NEW.c_program_id      IS DISTINCT FROM OLD.c_program_id;
  mod_existence       bool := NEW.c_existence       IS DISTINCT FROM OLD.c_existence;
  mod_name            bool := NEW.c_name            IS DISTINCT FROM OLD.c_name;
  mod_proposal_status bool := NEW.c_proposal_status IS DISTINCT FROM OLD.c_proposal_status;
BEGIN

  INSERT INTO t_chron_program_update AS chron (
    c_operation,
    c_program_id          ,
    c_mod_program_id      ,
    c_mod_existence       ,
    c_mod_name            ,
    c_mod_proposal_status ,
    c_new_program_id      ,
    c_new_existence       ,
    c_new_name            ,
    c_new_proposal_status
  ) VALUES (
    TG_OP::e_tg_op,
    coalesce(OLD.c_program_id, NEW.c_program_id),
    mod_program_id,
    mod_existence,
    mod_name,
    mod_proposal_status,
    CASE WHEN mod_program_id      = true THEN NEW.c_program_id      END,
    CASE WHEN mod_existence       = true THEN NEW.c_existence       END,
    CASE WHEN mod_name            = true THEN NEW.c_name            END,
    CASE WHEN mod_proposal_status = true THEN NEW.c_proposal_status END
  ) ON CONFLICT ON CONSTRAINT t_chron_program_update_unique DO UPDATE SET
    c_mod_existence       = chron.c_mod_existence       OR mod_existence,
    c_mod_name            = chron.c_mod_name            OR mod_name,
    c_mod_proposal_status = chron.c_mod_proposal_status OR mod_proposal_status,
    c_new_program_id      = CASE WHEN chron.c_mod_program_id      OR mod_program_id      THEN NEW.c_program_id    END,
    c_new_existence       = CASE WHEN chron.c_mod_existence       OR mod_existence       THEN NEW.c_existence     END,
    c_new_name            = CASE WHEN chron.c_mod_name            OR mod_name            THEN NEW.c_name          END,
    c_new_proposal_status = CASE WHEN chron.c_mod_proposal_status OR mod_proposal_status THEN NEW.c_proposal_status END;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;