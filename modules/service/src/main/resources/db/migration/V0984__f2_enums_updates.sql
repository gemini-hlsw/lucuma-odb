
--- Flamingos 2 enumerations updates
ALTER TABLE t_f2_decker
  DROP COLUMN c_obsolete;

ALTER TABLE t_f2_fpu
  DROP COLUMN c_obsolete;

delete from t_f2_lyot_wheel where c_tag = 'F32High';
delete from t_f2_lyot_wheel where c_tag = 'F32Low';
delete from t_f2_lyot_wheel where c_tag = 'F33Gems';

ALTER TABLE t_f2_filter
  DROP COLUMN c_obsolete;
