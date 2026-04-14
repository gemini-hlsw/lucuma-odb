
-- A view that facilitates the GHOST IFU mapping by adding embedded ids that are
-- null or non-null depending on camera type.
CREATE VIEW v_ghost_blue_exposure_time_mode AS
  SELECT
    e.*,
    CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
    CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id
  FROM t_exposure_time_mode e
  INNER JOIN t_ghost_ifu g ON g.c_blue_exposure_time_mode_id = e.c_exposure_time_mode_id;

CREATE VIEW v_ghost_red_exposure_time_mode AS
  SELECT
    e.*,
    CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
    CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id
  FROM t_exposure_time_mode e
  INNER JOIN t_ghost_ifu g ON g.c_red_exposure_time_mode_id = e.c_exposure_time_mode_id;