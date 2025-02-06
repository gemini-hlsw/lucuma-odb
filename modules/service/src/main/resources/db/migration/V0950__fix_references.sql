-- This adds schema qualification (i.e., public.foobar) to objects referenced in stored procedures.
-- This is important for restoring backups.

CREATE OR REPLACE FUNCTION format_gmos_long_slit_mode_group(
  site                  e_site,
  program_id            d_program_id,
  observing_mode_type   e_observing_mode_type,
  grating               d_tag,
  filter                d_tag,
  fpu                   d_tag,
  central_wavelength    d_wavelength_pm,
  xbin                  d_tag,
  xbin_default          d_tag,
  ybin                  d_tag,
  ybin_default          d_tag,
  amp_read_mode         d_tag,
  amp_gain              d_tag,
  roi                   d_tag,
  wavelength_dithers    text,
  spatial_offsets       text
) RETURNS text AS $$
DECLARE
  dither_nm smallint;
  formatted_spatial text;
BEGIN
  -- Figure out the default wavelength dither value.
  EXECUTE format(
    'SELECT c_wavelength_dither_nm FROM public.t_gmos_%I_disperser WHERE c_tag = $1',
    CASE
      WHEN site = 'gn' THEN 'north'
      WHEN site = 'gs' THEN 'south'
    END
  ) INTO dither_nm USING grating;

  -- Trim trailing zeros from the spatial offsets so that numerically equivalent
  -- values compare the same.
  IF spatial_offsets IS NOT NULL THEN
    formatted_spatial := regexp_replace(spatial_offsets, '(\.\d*?[1-9])0*|\.(0+)', '\1', 'g');
  ELSE
    formatted_spatial := '0,15,-15';
  END IF;

  -- Concat all the fields together into a text value.  Here we use the default
  -- value when an explicit override is not present.  For example, a mode that
  -- explicitly requests binning of 1x1 will compare the same as one that simply
  -- defaults to 1x1 based on the source profile and image quality, etc.
  RETURN concat_ws(
    ':',
    program_id::text,
    observing_mode_type::text,
    grating::text,
    COALESCE(filter::text, 'None'),
    fpu::text,
    central_wavelength::text,
    COALESCE(xbin, xbin_default)::text,
    COALESCE(ybin, ybin_default)::text,
    COALESCE(amp_read_mode::text, 'Slow'),
    COALESCE(amp_gain::text, 'Low'),
    COALESCE(roi::text, 'FullFrame'),
    COALESCE(
      wavelength_dithers,
      gmos_long_slit_default_wavelength_dither(dither_nm)
    ),
    formatted_spatial
  );
END;
$$ LANGUAGE plpgsql IMMUTABLE;



-- Update the entry point for formatting to handle COM and MON
-- Update the entry point for formatting to handle SYS
CREATE OR REPLACE FUNCTION format_program_reference(
  ptype           e_program_type,
  semester        d_semester,
  index           int4,
  proposal_status d_tag,
  science_subtype e_science_subtype,
  instrument      d_tag,
  description     text
)
RETURNS text AS $$
BEGIN
    RETURN CASE
      WHEN ptype = 'calibration'   OR
           ptype = 'commissioning' OR
           ptype = 'engineering'   OR
           ptype = 'monitoring'    THEN
          public.format_semester_instrument_reference(ptype, semester, index, instrument)

      WHEN ptype = 'example' OR
           ptype = 'library' THEN
          public.format_lib_or_xpl_reference(ptype, instrument, description)

      WHEN ptype = 'system' THEN
          CONCAT('SYS-', description)

      WHEN ptype = 'science' AND proposal_status = 'accepted' THEN
          public.format_science_reference(semester, index, science_subtype)

      ELSE
          NULL
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;
