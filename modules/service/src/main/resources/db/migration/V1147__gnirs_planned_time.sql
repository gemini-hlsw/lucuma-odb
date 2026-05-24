-- GNIRS time estimates.
-- Values ported from the OCS legacy code (ocs/bundle/edu.gemini.pot,
-- InstGNIRS.java and SPInstObsComp.java).

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
