--- FLAMINGOS 2 LONG SLIT OBSERVING MODE

create table t_flamingos_2_long_slit (

  c_observation_id             d_observation_id      NOT NULL,
  c_instrument                 d_tag NOT NULL DEFAULT 'Flamingos2' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'Flamingos2'),
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'flamingos_2_long_slit' check (c_observing_mode_type = 'flamingos_2_long_slit'),

  c_disperser                  d_tag                 NOT NULL             REFERENCES t_f2_disperser(c_tag),
  c_filter                     d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_filter(c_tag),
  c_fpu                        d_tag                 NOT NULL             REFERENCES t_f2_fpu(c_tag),
  c_read_mode                  d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_read_mode(c_tag),
  c_decker                     d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_decker(c_tag),
  c_readout_mode               d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_readout_mode(c_tag),
  c_reads                      d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_reads(c_tag),
  c_window_cover               d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_window_cover(c_tag),
  c_use_electronic_offsetting  BOOL                  NULL DEFAULT NULL,

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);
