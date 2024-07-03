-- Science band
CREATE TYPE e_science_band AS ENUM(
  'band1',
  'band2',
  'band3',
  'band4'
);

ALTER TABLE t_allocation
  ADD COLUMN c_science_band e_science_band;

UPDATE t_allocation
   SET c_science_band = 'band3';

ALTER TABLE t_allocation
  ALTER COLUMN c_science_band SET NOT NULL,
  DROP CONSTRAINT "t_allocation_c_program_id_c_partner_key",
  ADD CONSTRAINT "t_allocation_pid_partner_band"
    UNIQUE (c_program_id, c_partner, c_science_band);
