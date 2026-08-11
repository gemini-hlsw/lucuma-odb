-- GNIRS config change overheads.
--
-- V1148 noted that GNIRS in OCS declares no mechanism-change overheads (no
-- CONFIG_CHANGE entries in InstGNIRS.calc), and so seeded no gnirs_<mechanism>
-- rows.  That held while a GNIRS spectroscopy observation sat at a single
-- grating setting for its whole science sequence: the grating turret never
-- moved once the sequence started, so there was nothing to charge.
--
-- Observations may now take spectra at several central wavelengths, running
-- each as its own segment and returning to earlier ones, so the turret moves
-- repeatedly for the life of the observation.  That motion is charged here.
INSERT INTO t_time_estimate VALUES(
  'gnirs_wavelength',
  'GNIRS Wavelength',
  'GNIRS central wavelength change cost',
  'Gnirs',
  '10 seconds'
);

-- The filter wheel moves whenever consecutive steps use different filters.  That
-- happens in imaging, whose science sequence steps through the configured
-- filters, and also in spectroscopy: an acquisition images the field and then
-- the slit or IFU through different filters, even though the science sequence
-- itself stays on one.
INSERT INTO t_time_estimate VALUES(
  'gnirs_filter',
  'GNIRS Filter',
  'GNIRS filter change cost',
  'Gnirs',
  '10 seconds'
);
