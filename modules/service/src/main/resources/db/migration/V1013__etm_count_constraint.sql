-- Ensure that exposure time mode count is a positive int.

UPDATE t_observation
   SET c_etm_exp_count = 1
 WHERE c_etm_exp_count IS NOT NULL AND c_etm_exp_count <= 0;

ALTER TABLE t_observation
  ADD CONSTRAINT exp_time_mode_count_check CHECK (c_etm_exp_count > 0);