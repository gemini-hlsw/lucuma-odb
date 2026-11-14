-- One view per visible SCORPIO channel, in the shape of v_exposure_time_mode, so
-- each channel's exposure time mode can be joined on the observation id alone.

CREATE VIEW v_scorpio_g_exposure_time_mode AS
  SELECT
    e.*,
    CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
    CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id,
    CASE WHEN e.c_is_explicit                            THEN e.c_exposure_time_mode_id END AS c_explicit_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'signal_to_noise'
         THEN e.c_exposure_time_mode_id END AS c_explicit_signal_to_noise_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'time_and_count'
         THEN e.c_exposure_time_mode_id END AS c_explicit_time_and_count_id
  FROM t_exposure_time_mode e
  INNER JOIN t_scorpio_long_slit s ON s.c_g_exposure_time_mode_id = e.c_exposure_time_mode_id;

CREATE VIEW v_scorpio_r_exposure_time_mode AS
  SELECT
    e.*,
    CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
    CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id,
    CASE WHEN e.c_is_explicit                            THEN e.c_exposure_time_mode_id END AS c_explicit_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'signal_to_noise'
         THEN e.c_exposure_time_mode_id END AS c_explicit_signal_to_noise_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'time_and_count'
         THEN e.c_exposure_time_mode_id END AS c_explicit_time_and_count_id
  FROM t_exposure_time_mode e
  INNER JOIN t_scorpio_long_slit s ON s.c_r_exposure_time_mode_id = e.c_exposure_time_mode_id;

CREATE VIEW v_scorpio_i_exposure_time_mode AS
  SELECT
    e.*,
    CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
    CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id,
    CASE WHEN e.c_is_explicit                            THEN e.c_exposure_time_mode_id END AS c_explicit_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'signal_to_noise'
         THEN e.c_exposure_time_mode_id END AS c_explicit_signal_to_noise_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'time_and_count'
         THEN e.c_exposure_time_mode_id END AS c_explicit_time_and_count_id
  FROM t_exposure_time_mode e
  INNER JOIN t_scorpio_long_slit s ON s.c_i_exposure_time_mode_id = e.c_exposure_time_mode_id;

CREATE VIEW v_scorpio_z_exposure_time_mode AS
  SELECT
    e.*,
    CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
    CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id,
    CASE WHEN e.c_is_explicit                            THEN e.c_exposure_time_mode_id END AS c_explicit_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'signal_to_noise'
         THEN e.c_exposure_time_mode_id END AS c_explicit_signal_to_noise_id,
    CASE WHEN e.c_is_explicit AND e.c_exposure_time_mode = 'time_and_count'
         THEN e.c_exposure_time_mode_id END AS c_explicit_time_and_count_id
  FROM t_exposure_time_mode e
  INNER JOIN t_scorpio_long_slit s ON s.c_z_exposure_time_mode_id = e.c_exposure_time_mode_id;
