ALTER TABLE t_time_estimate
       DROP c_category;

DROP TABLE t_planned_time_category;

INSERT INTO t_time_estimate VALUES(
  'gmos_north_longslit_setup',
  'GMOS North Longslit Setup',
  'GMOS North longslit mode full setup cost',
  'GmosNorth',
  '16 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_reacquisition',
  'GMOS North Reacquisition',
  'Gmos North reacquisition cost',
  'GmosNorth',
  '5 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_longslit_setup',
  'GMOS South Longslit Setup',
  'GMOS South longslit mode full setup cost',
  'GmosSouth',
  '16 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_reacquisition',
  'GMOS South Reacquisition',
  'Gmos South reacquisition cost',
  'GmosSouth',
  '5 minutes'
);