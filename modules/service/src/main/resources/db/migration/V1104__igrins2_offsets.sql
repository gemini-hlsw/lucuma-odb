-- Add spatial offsets support for IGRINS-2 Long Slit
ALTER TABLE t_igrins_2_long_slit
  ADD COLUMN c_spatial_offsets text NULL;

ALTER TABLE t_igrins_2_long_slit
  ADD CONSTRAINT check_igrins2_offsets_format
  CHECK (c_spatial_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?){7}$');
