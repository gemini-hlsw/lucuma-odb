CREATE TYPE e_source_profile_type AS ENUM(
  'point',
  'uniform',
  'gaussian'
);

CREATE TYPE source_profile_summary AS (
  c_profile_type e_source_profile_type,
  c_fwhm         d_angle_µas
);

CREATE OR REPLACE FUNCTION extract_source_profile_summary(
  src_profile  jsonb
)
RETURNS source_profile_summary AS $$
DECLARE
  profile_type e_source_profile_type;
  fwhm         d_angle_µas;
BEGIN
  SELECT
    COALESCE(
      CASE WHEN src_profile->'point'    IS NOT NULL THEN 'point'::e_source_profile_type    END,
      CASE WHEN src_profile->'uniform'  IS NOT NULL THEN 'uniform'::e_source_profile_type  END,
      CASE WHEN src_profile->'gaussian' IS NOT NULL THEN 'gaussian'::e_source_profile_type END
    ),
    -- We only have a fwhm for gaussian profiles, otherwise it is null.
    CASE WHEN src_profile->'gaussian' ? 'fwhm'
         THEN (src_profile->'gaussian'->'fwhm'->>'microarcseconds')::d_angle_µas
         ELSE NULL
    END
  INTO profile_type, fwhm;

  IF profile_type IS NULL THEN
    RAISE EXCEPTION 'Invalid source profile: expected one of "point", "uniform", or "gaussian".';
  END IF;

  IF profile_type = 'gaussian' AND fwhm IS NULL THEN
    RAISE EXCEPTION 'Invalid source profile: gaussian profile with no fwhm value.';
  END IF;

  RETURN (profile_type, fwhm);
END;
$$ LANGUAGE plpgsql IMMUTABLE;