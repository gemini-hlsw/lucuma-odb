
-- Increase the width of the signal-to-noise column to fit an integer milli-sn
-- value.

do $$
  declare v_observation_def text;
  declare v_observation_cmd text;
  declare v_generator_params_def text;
  declare v_generator_params_cmd text;
begin
  v_observation_def := pg_get_viewdef('v_observation');
  DROP VIEW v_observation;

  v_generator_params_def := pg_get_viewdef('v_generator_params');
  DROP VIEW v_generator_params;

  ALTER TABLE t_observation
    ALTER COLUMN c_spec_signal_to_noise TYPE numeric(10,3);

  v_observation_cmd := format('CREATE VIEW v_observation AS %s', v_observation_def);
  EXECUTE v_observation_cmd;

  v_generator_params_cmd := format('CREATE VIEW v_generator_params AS %s', v_generator_params_def);
  EXECUTE v_generator_params_cmd;
end $$;