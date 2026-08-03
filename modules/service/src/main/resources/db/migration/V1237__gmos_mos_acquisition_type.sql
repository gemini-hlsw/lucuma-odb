-- Acquisition type: whether the acquisition image is taken with the mask in
-- the beam (imaging the alignment holes cut into the mask) or out of the beam.
-- There is no "automatic" state -- unlike GNIRS's acquisition type, this isn't
-- resolved from target brightness, so a MOS observation always carries an
-- explicit choice. MaskIn is the default, since aligning on the holes cut into
-- the mask is the usual way a MOS observation is acquired.
CREATE TYPE e_gmos_mos_acquisition_type AS ENUM ('MaskIn', 'MaskOut');

ALTER TABLE t_gmos_north_mos
  ADD COLUMN c_acquisition_type e_gmos_mos_acquisition_type NOT NULL DEFAULT 'MaskIn';

ALTER TABLE t_gmos_south_mos
  ADD COLUMN c_acquisition_type e_gmos_mos_acquisition_type NOT NULL DEFAULT 'MaskIn';

-- V1235 defined the views as `SELECT m.*`, which Postgres expands at creation
-- time, so they must be recreated to see the new column.
DROP VIEW v_gmos_north_mos;
DROP VIEW v_gmos_south_mos;

CREATE VIEW v_gmos_north_mos AS
  SELECT m.* FROM t_gmos_north_mos m;

CREATE VIEW v_gmos_south_mos AS
  SELECT m.* FROM t_gmos_south_mos m;
