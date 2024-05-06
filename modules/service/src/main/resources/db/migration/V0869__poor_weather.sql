ALTER TYPE e_science_subtype ADD VALUE 'poor_weather'    BEFORE 'queue';

ALTER TABLE t_science_subtype
  DROP CONSTRAINT t_science_subtype_c_abbr_check,
  ADD CONSTRAINT  t_science_subtype_c_abbr_check CHECK (c_abbr IN ('C', 'D', 'F', 'L', 'P', 'Q', 'S', 'V'));