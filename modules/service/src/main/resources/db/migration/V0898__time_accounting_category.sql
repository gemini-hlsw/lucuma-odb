-- This was created before `t_allocation` but was never actually used.
DROP TABLE t_time_allocation;

-- New time accounting category table.
CREATE TABLE t_time_accounting_category (
  c_tag          d_tag    PRIMARY KEY,
  c_description  varchar  NOT NULL
);

INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('ar',   'Argentina');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('br',   'Brazil');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('ca',   'Canada');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('cfht', 'CFHT Exchange');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('cl',   'Chile');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('dd',   'Director''s Time');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('ds',   'Demo Science');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('gt',   'Guaranteed Time');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('jp',   'Subaru');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('keck', 'Keck Exchange');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('kr',   'Republic of Korea');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('lp',   'Large Program');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('ltp',  'Limited-term Participant');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('sv',   'System Verification');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('uh',   'University of Hawaii');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('us',   'United States');

-- Add a time accounting category column to the allocation table.
ALTER TABLE t_allocation
  DROP CONSTRAINT "t_allocation_pid_partner_band",
  ADD COLUMN c_ta_category d_tag REFERENCES t_time_accounting_category(c_tag);

-- Initialize the time accounting category from the partner.  All partners
-- happen to have the same tag as the corresponding time accounting category.
UPDATE t_allocation
   SET c_ta_category = c_partner;

-- Make the category not null, and introduce a unique constraint for (pid,
-- category, band)
ALTER TABLE t_allocation
  ALTER COLUMN c_ta_category SET NOT NULL,
  ADD CONSTRAINT "t_allocation_pid_category_band"
    UNIQUE (c_program_id, c_ta_category, c_science_band),
  DROP COLUMN c_partner;