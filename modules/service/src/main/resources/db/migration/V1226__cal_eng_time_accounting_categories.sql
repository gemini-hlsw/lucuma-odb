-- Add the 'cal' (calibration) and 'eng' (engineering) time accounting
-- categories, matching the corresponding lucuma-core enum additions.
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('cal', 'Calibration');
INSERT INTO t_time_accounting_category (c_tag, c_description) VALUES('eng', 'Engineering');
