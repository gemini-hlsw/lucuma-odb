alter type e_observing_mode_type add value 'flamingos_2_long_slit';

--- FLAMINGOS 2 LONG SLIT OBSERVING MODE

create table t_flamingos_2_long_slit (

  c_observation_id             d_observation_id      NOT NULL,
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'flamingos_2_long_slit' check (c_observing_mode_type = 'flamingos_2_long_slit'),

  c_disperser                  d_tag                 NOT NULL             REFERENCES t_f2_disperser(c_tag),
  c_filter                     d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_filter(c_tag),
  c_fpu                        d_tag                 NOT NULL             REFERENCES t_f2_fpu(c_tag),
  c_read_mode                  d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_read_mode(c_tag),

  CONSTRAINT wavelength_dither_format CHECK (c_wavelength_dithers ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),
  CONSTRAINT offset_format            CHECK (c_spatial_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),

  PRIMARY KEY (c_observation_id, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);
