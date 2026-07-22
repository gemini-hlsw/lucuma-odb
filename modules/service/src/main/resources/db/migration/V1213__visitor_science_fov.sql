-- Add a separate science field-of-view column to t_visitor and set the
-- canonical per-mode values for the science field of view and the AGS
-- (acquisition & guide star) diameter. These were previously conflated in a
-- single column (c_ags_diameter); they are distinct quantities.

ALTER TABLE t_visitor ADD COLUMN c_science_fov_diameter d_angle_µas;

-- Alopeke & Zorro speckle:     science FoV 3"  (3000000 µas), AGS diameter 30" (30000000 µas)
UPDATE t_visitor
  SET c_science_fov_diameter = 3000000, c_ags_diameter = 30000000
  WHERE c_observing_mode_type IN ('alopeke_speckle', 'zorro_speckle');

-- Alopeke & Zorro wide-field:  science FoV 35" (35000000 µas), AGS diameter 60" (60000000 µas)
UPDATE t_visitor
  SET c_science_fov_diameter = 35000000, c_ags_diameter = 60000000
  WHERE c_observing_mode_type IN ('alopeke_wide_field', 'zorro_wide_field');

-- MaroonX:                     science FoV 0.77" (770000 µas), AGS diameter 60" (60000000 µas, a 30" radius)
UPDATE t_visitor
  SET c_science_fov_diameter = 770000, c_ags_diameter = 60000000
  WHERE c_observing_mode_type = 'maroon_x';

-- Generic visitor modes have no instrument-specific field of view; the science
-- FoV matches the (user-supplied) AGS diameter.
UPDATE t_visitor
  SET c_science_fov_diameter = c_ags_diameter
  WHERE c_observing_mode_type IN ('visitor_north', 'visitor_south');

-- Flush the deferred trigger events queued by the UPDATEs above so the
-- following ALTER TABLE is not blocked
SET CONSTRAINTS ALL IMMEDIATE;

ALTER TABLE t_visitor ALTER COLUMN c_science_fov_diameter SET NOT NULL;

COMMENT ON COLUMN t_visitor.c_science_fov_diameter IS
  'Science field of view, understood as the diameter of a circular area.';

-- Recreate v_visitor so its `v.*` picks up the new column.
DROP VIEW v_visitor;

CREATE VIEW v_visitor AS
  SELECT
    v.*,
    CASE WHEN v.c_total_request_time IS NOT NULL THEN v.c_observation_id END
      AS c_total_request_time_id
  FROM t_visitor v;
