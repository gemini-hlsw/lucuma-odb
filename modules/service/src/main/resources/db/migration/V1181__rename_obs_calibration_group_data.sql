-- Data migration following the "telluric group" -> "per-observation
-- calibration group" rename.

-- (a) Rename existing per-observation calibration group display names from
--     "<mode>/telluric/<oid>" to "<mode>/calibration/<oid>".  Names embed the
--     observation id, so the 1:1 substitution preserves the
--     (c_program_id, c_name) uniqueness of system groups.  The per-program
--     "Calibrations" group is not matched (no '/telluric/' segment).
UPDATE t_group
SET    c_name = regexp_replace(c_name, '/telluric/', '/calibration/')
WHERE  c_system = true
  AND  c_calibration_roles && ARRAY['telluric','daytime_pinhole']::e_calibration_role[]
  AND  c_name LIKE '%/telluric/%';

-- (b) Backfill 'daytime_pinhole' into the roles of existing GNIRS cross-dispersed
--     calibration groups (those that already contain a daytime pinhole flat), so
--     a group's c_calibration_roles reflects what it actually holds.
UPDATE t_group g
SET    c_calibration_roles = array_append(g.c_calibration_roles, 'daytime_pinhole'::e_calibration_role)
WHERE  g.c_system = true
  AND  'telluric' = ANY (g.c_calibration_roles)
  AND  NOT ('daytime_pinhole' = ANY (g.c_calibration_roles))
  AND  EXISTS (
    SELECT 1 FROM t_observation o
    WHERE  o.c_group_id = g.c_group_id
      AND  o.c_calibration_role = 'daytime_pinhole'
      AND  o.c_existence = 'present'
  );
