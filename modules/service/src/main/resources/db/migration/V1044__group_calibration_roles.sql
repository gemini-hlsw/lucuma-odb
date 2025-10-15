-- Add calibration roles support to groups
-- This allows system groups to indicate which types of calibrations they manage

-- Add the calibration roles column
ALTER TABLE t_group
ADD COLUMN c_calibration_roles e_calibration_role[] NOT NULL DEFAULT '{}'::e_calibration_role[];

COMMENT ON COLUMN t_group.c_calibration_roles IS
'Calibration roles supported by this group (system groups only). Empty array for non-calibration groups. Used to indicate which types of calibrations this group manages (e.g., Telluric, SpectroPhotometric). Only settable by system code, not user-editable.';

-- Recreate the v_group view to include the new column
DROP VIEW v_group;

CREATE VIEW v_group AS
  SELECT *,
  CASE WHEN c_min_interval IS NOT NULL THEN c_group_id END AS c_min_interval_id,
  CASE WHEN c_max_interval IS NOT NULL THEN c_group_id END AS c_max_interval_id
  FROM t_group;
