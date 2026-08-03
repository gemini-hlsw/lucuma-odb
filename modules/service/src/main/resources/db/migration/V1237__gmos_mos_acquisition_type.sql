-- Acquisition type: whether the acquisition image is taken with the mask in or out.
CREATE TYPE e_gmos_mos_acquisition_type AS ENUM ('MaskIn', 'MaskOut');

ALTER TABLE t_gmos_north_mos
  ADD COLUMN c_acquisition_type e_gmos_mos_acquisition_type NOT NULL DEFAULT 'MaskIn';

ALTER TABLE t_gmos_south_mos
  ADD COLUMN c_acquisition_type e_gmos_mos_acquisition_type NOT NULL DEFAULT 'MaskIn';

-- Reconstruct the views
DROP VIEW v_gmos_north_mos;
DROP VIEW v_gmos_south_mos;

CREATE VIEW v_gmos_north_mos AS
  SELECT m.* FROM t_gmos_north_mos m;

CREATE VIEW v_gmos_south_mos AS
  SELECT m.* FROM t_gmos_south_mos m;
