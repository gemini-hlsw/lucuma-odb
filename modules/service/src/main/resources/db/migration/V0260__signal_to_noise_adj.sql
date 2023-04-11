
-- Increase the width of the signal-to-noise column to fit a numeric(10,3) value.
-- Requires dropping and then recreating views based on the observation table.

CALL update_table_with_views(
  't_observation',
  'ALTER TABLE t_observation ALTER COLUMN c_spec_signal_to_noise TYPE numeric(10,3)',
  'v_observation',
  'v_generator_params'
);