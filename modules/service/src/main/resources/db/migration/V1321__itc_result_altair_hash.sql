-- Key the Altair parameters of a cached ITC result apart from the input hash.
--
-- c_hash is the md5 of the ITC input as the database determines it. Behind Altair the ITC is
-- additionally modelled from the guide star the observation will use, which only sequence
-- generation resolves: a reader that has the database alone (the workflow validator, obscalc)
-- cannot reproduce it and would never match a hash that folded it in, and would report the ITC
-- result of every Altair observation as missing. The Altair parameters are therefore left out of
-- c_hash and recorded here instead, null when the ITC was called without Altair.
--
-- As with c_hash and c_is_frozen (see V1225) the one column covers both parts of the result: the
-- science and the acquisition calculations are made with the same Altair parameters.
ALTER TABLE t_itc_result
  ADD COLUMN c_altair_hash bytea NULL;

COMMENT ON COLUMN t_itc_result.c_altair_hash IS
  'md5 of the Altair parameters the ITC was called with; null when it was called without Altair.';
