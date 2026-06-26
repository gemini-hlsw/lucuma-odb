-- Truncate to allow schema change (c_asterism_results was NOT NULL).
TRUNCATE TABLE t_itc_result;

-- Make c_asterism_results nullable and add c_error_message for deterministic failures.
-- Exactly one of the two must be non-null (success vs. cached deterministic failure).
ALTER TABLE t_itc_result
  ALTER COLUMN c_asterism_results DROP NOT NULL,
  ADD COLUMN c_error_message text,
  ADD CONSTRAINT t_itc_result_one_result CHECK (num_nonnulls(c_asterism_results, c_error_message) = 1);
