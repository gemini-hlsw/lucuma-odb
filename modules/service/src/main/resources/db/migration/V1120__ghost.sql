
-- A view that facilitates the GHOST IFU mapping by separating the blue and
-- red channels.  Otherwise, the views are identical to v_exposure_time_mode.

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