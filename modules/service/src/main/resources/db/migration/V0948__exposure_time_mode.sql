-- Set the exposure time mode appropriately in existing observations.
UPDATE t_observation
   SET c_spec_exp_time_mode = 'signal_to_noise'
 WHERE c_spec_signal_to_noise    IS NOT NULL
   AND c_spec_signal_to_noise_at IS NOT NULL;