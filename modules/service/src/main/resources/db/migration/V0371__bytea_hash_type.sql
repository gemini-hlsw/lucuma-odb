TRUNCATE TABLE t_itc_result;

ALTER TABLE t_itc_result
  ALTER COLUMN c_hash TYPE bytea USING c_hash::bytea;

TRUNCATE TABLE t_execution_digest;

ALTER TABLE t_execution_digest
  ALTER COLUMN c_hash TYPE bytea USING c_hash::bytea;