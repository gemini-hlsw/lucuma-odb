TRUNCATE TABLE t_execution_digest;

ALTER TABLE t_execution_digest
  ADD COLUMN c_setup_count int4 NOT NULL CHECK (c_setup_count >= 0);

TRUNCATE TABLE t_obscalc;

ALTER TABLE t_obscalc
  ADD COLUMN c_setup_count int4 NULL CHECK (c_setup_count >= 0);

INSERT INTO t_time_estimate VALUES(
  'f2_longslit_max_visit',
  'Flamingos 2 Longslit Max Visit',
  'Flamingos 2 Longslit Max Visit',
  'Flamingos2',
  '2 hours'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_imaging_max_visit',
  'GMOS North Imaging Max Visit',
  'GMOS North Imaging Max Visit',
  'GmosNorth',
  '2 hours'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_imaging_setup',
  'GMOS North Imaging Setup',
  'GMOS North Imaging mode full setup cost',
  'GmosNorth',
  '6 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_longslit_max_visit',
  'GMOS North Longslit Max Visit',
  'GMOS North Longslit Max Visit',
  'GmosNorth',
  '2 hours'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_imaging_max_visit',
  'GMOS South Imaging Max Visit',
  'GMOS South Imaging Max Visit',
  'GmosSouth',
  '2 hours'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_imaging_setup',
  'GMOS South Imaging Setup',
  'GMOS South Imaging mode full setup cost',
  'GmosSouth',
  '6 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_longslit_max_visit',
  'GMOS South Longslit Max Visit',
  'GMOS South Longslit Max Visit',
  'GmosSouth',
  '2 hours'
);

INSERT INTO t_time_estimate VALUES(
  'igrins2_longslit_max_visit',
  'IGRINS-2 Longslit Max Visit',
  'IGRINS-2 Longslit Max Visit',
  'Igrins2',
  '2 hours'
);