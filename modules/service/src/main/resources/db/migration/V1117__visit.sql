CREATE TYPE e_visit_origin AS ENUM(
  'slew',
  'observe'
);

ALTER TABLE t_visit
  ADD COLUMN c_origin e_visit_origin NOT NULL DEFAULT 'observe';