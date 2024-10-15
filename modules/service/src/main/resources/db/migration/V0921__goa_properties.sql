ALTER TABLE t_program
  RENAME COLUMN c_proprietary TO c_goa_proprietary;

ALTER TABLE t_program
  ADD COLUMN c_goa_should_notify  bool NOT NULL DEFAULT true,
  ADD COLUMN c_goa_private_header bool NOT NULL DEFAULT false;

COMMENT ON COLUMN t_program.c_goa_should_notify IS 'Whether the GOA should send notifications for new datasets.';

COMMENT ON COLUMN t_program.c_goa_private_header IS 'Whether the header should be kept private as well during the proprietary period.';