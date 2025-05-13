-- A function to get the instrument string to use in a program reference given
-- a particular instrument.

CREATE OR REPLACE FUNCTION instrument_reference_name(
  instrument d_tag
)
RETURNS text AS $$
DECLARE
  name text;
BEGIN
  name := CASE
    WHEN instrument = 'AcqCam'     THEN 'ACQCAM'
    WHEN instrument = 'Alopeke'    THEN 'ALOPEKE'
    WHEN instrument = 'Flamingos2' THEN 'F2'
    WHEN instrument = 'Ghost'      THEN 'GHOST'
    WHEN instrument = 'GmosNorth'  THEN 'GMOSN'
    WHEN instrument = 'GmosSouth'  THEN 'GMOSS'
    WHEN instrument = 'Gnirs'      THEN 'GNIRS'
    WHEN instrument = 'Gpi'        THEN 'GPI'
    WHEN instrument = 'Gsaoi'      THEN 'GSAOI'
    WHEN instrument = 'Igrins2'    THEN 'IGRINS2'
    WHEN instrument = 'Niri'       THEN 'NIRI'
    WHEN instrument = 'Scorpio'    THEN 'SCORPIO'
    WHEN instrument = 'Visitor'    THEN 'VISITOR'
    WHEN instrument = 'Zorro'      THEN 'ZORRO'
  END;

  IF name IS NULL THEN
    RAISE EXCEPTION 'Unknown instrument: %', instrument::text
      USING HINT = 'Please update instrument_reference_name function';
  END IF;

  RETURN name;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Update functions that previously looked up this value from t_instrument to
-- instead use this function.

CREATE OR REPLACE FUNCTION format_semester_instrument_reference(ptype e_program_type, semester d_semester, index int4, instrument d_tag)
RETURNS text AS $$
DECLARE
  abbr text;
  inst text;
BEGIN
    abbr := public.program_type_abbr(ptype);
    inst := public.instrument_reference_name(instrument);
    RETURN CONCAT('G-', semester, '-', abbr, '-', inst, '-', LPAD(index::text, 2, '0'));
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION format_lib_or_xpl_reference(ptype e_program_type, instrument d_tag, description text)
RETURNS text AS $$
DECLARE
  abbr text;
  inst text;
  prefix text;
BEGIN
    abbr   := public.program_type_abbr(ptype);
    inst   := public.instrument_reference_name(instrument);
    prefix := CONCAT('G-', abbr, '-', inst);

    RETURN  CASE
      WHEN ptype = 'library' THEN CONCAT(prefix, '-', description)
      ELSE prefix
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- We no longer maintain the reference in the instrument table.
ALTER TABLE t_instrument
  DROP COLUMN c_reference_name;

-- We were missing IGrins2 in the instrument table!
INSERT INTO t_instrument VALUES (
  'Igrins2', 'IGRINS2', 'IGRINS2'
);