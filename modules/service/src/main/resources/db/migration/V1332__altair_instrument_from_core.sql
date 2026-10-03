-- Which instruments support Altair is now decided by lucuma-core (Instrument.supportsAltair).
DROP TRIGGER trigger_t_observation_altair_instrument ON t_observation;
DROP FUNCTION check_altair_instrument();

ALTER TABLE t_instrument
  DROP COLUMN c_altair;
