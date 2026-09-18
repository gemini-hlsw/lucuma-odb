-- GNIRS setup costs per mode, as confirmed by the GNIRS team: spectroscopy keeps
-- the OCS values (InstGNIRS.SETUP_TIME_IFU = 20 minutes; long slit stays at the
-- existing 15), while imaging may be reduced to 10 minutes. Until now imaging
-- and the IFU were charged the long slit cost.

INSERT INTO t_time_estimate VALUES(
  'gnirs_ifu_setup',
  'GNIRS IFU Setup',
  'GNIRS IFU mode full setup cost',
  'Gnirs',
  '20 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'gnirs_imaging_setup',
  'GNIRS Imaging Setup',
  'GNIRS imaging mode full setup cost',
  'Gnirs',
  '10 minutes'
);
