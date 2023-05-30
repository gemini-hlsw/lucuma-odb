UPDATE t_observation
   SET c_instrument = 'GmosNorth'
 WHERE c_observing_mode_type = 'gmos_north_long_slit';

UPDATE t_observation
   SET c_instrument = 'GmosSouth'
 WHERE c_observing_mode_type = 'gmos_south_long_slit';

ALTER TABLE t_gmos_north_long_slit
        ADD       c_instrument d_tag NOT NULL DEFAULT 'GmosNorth' REFERENCES t_instrument(c_tag),
        ADD CHECK (c_instrument = 'GmosNorth'),
  DROP CONSTRAINT t_gmos_north_long_slit_pkey,
  DROP CONSTRAINT t_gmos_north_long_slit_c_observation_id_c_observing_mode_t_fkey;

ALTER TABLE t_gmos_south_long_slit
        ADD       c_instrument d_tag NOT NULL DEFAULT 'GmosSouth' REFERENCES t_instrument(c_tag),
        ADD CHECK (c_instrument = 'GmosSouth'),
  DROP CONSTRAINT t_gmos_south_long_slit_pkey,
  DROP CONSTRAINT t_gmos_south_long_slit_c_observation_id_c_observing_mode_t_fkey;

ALTER TABLE t_observation
  DROP CONSTRAINT t_observation_c_observation_id_c_observing_mode_type_key,
  ADD UNIQUE (c_observation_id, c_instrument, c_observing_mode_type);

ALTER TABLE t_gmos_north_long_slit
  ADD PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
   ADD CONSTRAINT t_gmos_north_long_slit_obs_fkey
      FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
       REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
                  ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_gmos_south_long_slit
  ADD PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
   ADD CONSTRAINT t_gmos_south_long_slit_obs_fkey
      FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
       REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
                  ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;