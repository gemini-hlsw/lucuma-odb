UPDATE t_visit
  SET c_raw_program_time   = c_raw_program_time   + c_raw_partner_time,
      c_final_program_time = c_final_program_time + c_final_partner_time;

ALTER TABLE t_visit
  DROP COLUMN c_raw_partner_time,
  DROP COLUMN c_final_partner_time;

UPDATE t_time_charge_discount
  SET c_program_discount = c_program_discount + c_partner_discount;

ALTER TABLE t_time_charge_discount
  DROP COLUMN c_partner_discount;

UPDATE t_execution_digest
  SET c_acq_program_time = c_acq_program_time + c_acq_partner_time,
      c_sci_program_time = c_sci_program_time + c_sci_partner_time;

ALTER TABLE t_execution_digest
  DROP COLUMN c_acq_partner_time,
  DROP COLUMN c_sci_partner_time;