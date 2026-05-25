-- GNIRS time estimates.
--
-- Sourced from the OCS legacy code (ocs/bundle/edu.gemini.pot,
-- InstGNIRS.java + SPInstObsComp.java + GnirsReadoutTime.java).
--
-- Per-step overhead structure for GNIRS in OCS (InstGNIRS.calc):
--   * READOUT     — per-coadd × coadds; encoded in lucuma-core's
--                   `GnirsReadMode.readoutTimePerCoadd` (no DB entry needed).
--   * DHS_WRITE   — 8500 ms; `gnirs_write` below.
--   * Offset / Gcal — added by the generic CommonStepCalculator (handled by
--                     the lucuma-odb `ConfigChangeEstimator` base trait).
--
-- Notably, GNIRS in OCS does NOT declare any mechanism-change overheads
-- (no Filter/FPU/Disperser/Camera/Decker/Prism CONFIG_CHANGE entries),
-- unlike GMOS, Flamingos-2, NIRI, GSAOI, GPI, and NICI. That's why
-- `ConfigChangeEstimator.gnirs.instrumentChecks` returns Nil and there are
-- no `gnirs_<mechanism>` rows here.

-- InstGNIRS.SETUP_TIME = 15 minutes (LongSlit non-LGS, non-IFU).
INSERT INTO t_time_estimate VALUES(
  'gnirs_longslit_setup',
  'GNIRS Longslit Setup',
  'GNIRS longslit mode full setup cost',
  'Gnirs',
  '15 minutes'
);

-- InstGNIRS.REACQUISITION_TIME = 6 minutes (REL-1346).
INSERT INTO t_time_estimate VALUES(
  'gnirs_reacquisition',
  'GNIRS Reacquisition',
  'GNIRS reacquisition cost',
  'Gnirs',
  '6 minutes'
);

-- SPInstObsComp.DHS_WRITE_TIMES[Gnirs] = 8500 ms.
INSERT INTO t_time_estimate VALUES(
  'gnirs_write',
  'GNIRS Write',
  'GNIRS write out time',
  'Gnirs',
  '8500 milliseconds'
);

-- Max-visit has no direct OCS equivalent. Used by the setup-count estimator
-- to decide how often to reapply the setup cost over the total science time.
-- Mirroring the IGRINS-2 default of 2 hours; instrument scientists can tune.
INSERT INTO t_time_estimate VALUES(
  'gnirs_longslit_max_visit',
  'GNIRS Longslit Max Visit',
  'GNIRS Longslit Max Visit',
  'Gnirs',
  '2 hours'
);

-- Setup variants from OCS (InstGNIRS.SETUP_TIME_LGS = 25 min,
-- InstGNIRS.SETUP_TIME_IFU = 20 min) are intentionally NOT inserted here:
-- those modes (LGS, IFU) are not yet implemented in lucuma-odb.  Add them
-- alongside the corresponding observing-mode work.
