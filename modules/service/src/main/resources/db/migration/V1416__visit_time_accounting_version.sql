-- The version of the time accounting rules that apply to each visit.  It is
-- fixed when the visit is created, so that a later change to the rules does
-- not reprice visits that were charged under earlier ones, even when they are
-- recomputed for some other reason (e.g., a dataset QA state change).
--
--   1. The rules in place before this migration.
--   2. A visit in which no science dataset passes QA is not charged at all.
--      In version 1, only the atoms with failing datasets are discounted, so
--      time between atoms (e.g., while the sequence is paused) is still
--      charged.
--
-- Existing visits keep version 1 and new visits get version 2.

ALTER TABLE t_visit
  ADD COLUMN c_ta_version smallint NOT NULL DEFAULT 1 CHECK (c_ta_version >= 1);

ALTER TABLE t_visit
  ALTER COLUMN c_ta_version SET DEFAULT 2;

COMMENT ON COLUMN t_visit.c_ta_version IS
  'The version of the time accounting rules that apply to this visit, fixed when the visit is created.';
