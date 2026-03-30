-- t_igrins_2_dynamic  needs to reference t_step instead of t_step_record,
ALTER TABLE t_igrins_2_dynamic
  DROP CONSTRAINT t_igrins_2_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_igrins_2_dynamic
  ADD CONSTRAINT t_igrins_2_dynamic_c_step_id_c_instrument_fkey
    FOREIGN KEY (c_step_id, c_instrument)
    REFERENCES t_step(c_step_id, c_instrument)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;
