
-- A view that facilitates the GHOST IFU mapping by adding embedded ids that are
-- null or non-null depending on camera type.
CREATE VIEW v_ghost_exposure_time_mode_link AS
  SELECT
     e.c_exposure_time_mode_id,
     CASE WHEN g.c_blue_exposure_time_mode_id = e.c_exposure_time_mode_id THEN e.c_observation_id END AS c_blue_observation_id,
     CASE WHEN g.c_red_exposure_time_mode_id  = e.c_exposure_time_mode_id THEN e.c_observation_id END AS c_red_observation_id
  FROM t_exposure_time_mode e
  INNER JOIN t_ghost_ifu g
    ON g.c_observation_id = e.c_observation_id
    AND (
         g.c_blue_exposure_time_mode_id = e.c_exposure_time_mode_id
      OR g.c_red_exposure_time_mode_id  = e.c_exposure_time_mode_id
    );