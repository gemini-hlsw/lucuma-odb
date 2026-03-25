-- Adds an acquisition configuration to Flamingos 2 Long Slit.
-- It consists of an acquisition filter and exposure time mode override.
-- The exposure time mode is already stored in t_exposure_time_mode and
-- populated from new (to this PR) inputs.

-- First we mark the acquisition filters in the database.
ALTER TABLE t_f2_filter
  ADD COLUMN c_is_acquisition_filter boolean NOT NULL DEFAULT FALSE;

UPDATE t_f2_filter
  SET c_is_acquisition_filter = TRUE
  WHERE c_tag IN ('J', 'H', 'KShort');

-- Add the acquisition filter override.
ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_acquisition_filter d_tag REFERENCES t_f2_filter(c_tag);

-- Adds the acquisition filter default calculation to the F2 Long Slit view.
DROP VIEW v_flamingos_2_long_slit;
CREATE OR REPLACE VIEW v_flamingos_2_long_slit AS
SELECT
  m.*,
  (
    SELECT af.c_tag
      FROM t_f2_filter af
      JOIN t_f2_filter sf ON sf.c_tag = m.c_filter
      WHERE af.c_is_acquisition_filter
      ORDER BY abs(af.c_wavelength - sf.c_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default
FROM t_flamingos_2_long_slit m;