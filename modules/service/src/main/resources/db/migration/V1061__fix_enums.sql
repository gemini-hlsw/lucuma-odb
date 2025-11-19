-- update enums to match the sequence

-- add 'import' catalog name
ALTER TYPE e_catalog_name ADD VALUE 'import';

--
-- remove 'partner' charge class
--

DELETE FROM t_time_charge_correction WHERE c_charge_class = 'partner'; -- there shouldn't be any

CREATE TYPE e_charge_class_temp AS ENUM (
  'nonCharged',
  'program'
);

ALTER TABLE t_time_charge_correction
ALTER COLUMN c_charge_class SET DATA TYPE e_charge_class_temp
USING c_charge_class::text::e_charge_class_temp;

DROP TYPE e_charge_class;

ALTER TYPE e_charge_class_temp RENAME TO e_charge_class;

--
-- remove 'slew' from sequence command
--

UPDATE t_execution_event SET c_sequence_command = NULL WHERE c_sequence_command = 'slew';

CREATE TYPE e_sequence_command_command AS ENUM(
  'abort',
  'continue',
  'pause',
  'start',
  'stop'
);

ALTER TABLE t_execution_event
ALTER COLUMN c_sequence_command SET DATA TYPE e_sequence_command_command
USING c_sequence_command::text::e_sequence_command_command;

DROP TYPE e_sequence_command;

ALTER TYPE e_sequence_command_command RENAME TO e_sequence_command;
