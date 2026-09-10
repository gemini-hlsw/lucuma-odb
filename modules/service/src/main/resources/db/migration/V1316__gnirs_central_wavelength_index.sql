-- GNIRS spectroscopy central wavelengths become an ordered, possibly duplicated list.  The
-- user's order is the order the sequence executes them in, and two entries may share a
-- wavelength while carrying different exposure time modes and coadds -- each is an
-- independent configuration with its own ITC result and its own sequence segment.
--
-- The wavelength therefore stops being part of the row identity; an explicit 0-based index
-- takes its place.

ALTER TABLE t_gnirs_central_wavelength_config
  ADD COLUMN c_index smallint;

-- Backfill.  Existing rows were stored (and executed) in increasing wavelength order, so
-- number them that way and nothing changes for existing observations.  The join is on the
-- outgoing primary key, which is still unique at this point.
UPDATE t_gnirs_central_wavelength_config w
   SET c_index = n.c_index
  FROM (
    SELECT
      c_observation_id,
      c_central_wavelength,
      c_version,
      (row_number() OVER (
         PARTITION BY c_observation_id, c_version
         ORDER BY c_central_wavelength
       ) - 1)::smallint AS c_index
    FROM t_gnirs_central_wavelength_config
  ) n
 WHERE w.c_observation_id     = n.c_observation_id
   AND w.c_central_wavelength = n.c_central_wavelength
   AND w.c_version            = n.c_version;

ALTER TABLE t_gnirs_central_wavelength_config
  ALTER COLUMN c_index SET NOT NULL,
  ADD CONSTRAINT t_gnirs_central_wavelength_config_index_check CHECK (c_index >= 0);

-- The index replaces the wavelength in the primary key.  The UNIQUE (c_exposure_time_mode_id)
-- constraint is unchanged: each entry still backs exactly one exposure time mode row.
ALTER TABLE t_gnirs_central_wavelength_config
  DROP CONSTRAINT t_gnirs_central_wavelength_config_pkey;

ALTER TABLE t_gnirs_central_wavelength_config
  ADD PRIMARY KEY (c_observation_id, c_version, c_index);

COMMENT ON TABLE t_gnirs_central_wavelength_config IS
  'GNIRS spectroscopy central wavelengths in user-specified order, each with its own exposure time mode and coadds.  A wavelength may repeat; each entry is an independent configuration.';

COMMENT ON COLUMN t_gnirs_central_wavelength_config.c_index IS
  '0-based position in the user-specified central wavelength list; the order the sequence executes them in.';

-- V1248 created this table without an obscalc invalidation trigger (V1301 installs those on
-- mode tables only).  Reordering the list is now a meaningful edit that changes the generated
-- sequence, so make sure it invalidates even if nothing else about the observation changed.
CREATE TRIGGER gnirs_central_wavelength_config_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_gnirs_central_wavelength_config
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();
