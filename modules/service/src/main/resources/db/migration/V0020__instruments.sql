
--- INSTRUMENTS

CREATE TABLE t_instrument (
  c_tag        d_tag  NOT NULL PRIMARY KEY,
  c_short_name VARCHAR,
  c_long_name  VARCHAR
);
comment on table t_instrument is 'Lookup table for instruments.';

insert into t_instrument values('AcqCam', 'AcqCam', 'Acquisition Camera');
insert into t_instrument values('Bhros', 'bHROS', 'bHROS');
insert into t_instrument values('Flamingos2', 'Flamingos2', 'Flamingos 2');
insert into t_instrument values('Ghost', 'GHOST', 'GHOST');
insert into t_instrument values('GmosNorth', 'GMOS-N', 'GMOS North');
insert into t_instrument values('GmosSouth', 'GMOS-S', 'GMOS South');
insert into t_instrument values('Gnirs', 'GNIRS', 'GNIRS');
insert into t_instrument values('Gpi', 'GPI', 'GPI');
insert into t_instrument values('Gsaoi', 'GSAOI', 'GSAOI');
insert into t_instrument values('Michelle', 'Michelle', 'Michelle');
insert into t_instrument values('Nici', 'NICI', 'NICI');
insert into t_instrument values('Nifs', 'NIFS', 'NIFS');
insert into t_instrument values('Niri', 'NIRI', 'NIRI');
insert into t_instrument values('Phoenix', 'Phoenix', 'Phoenix');
insert into t_instrument values('Trecs', 'TReCS', 'TReCS');
insert into t_instrument values('Visitor', 'Visitor Instrument', 'Visitor Instrument');
insert into t_instrument values('Scorpio', 'SCORPIO', 'Scorpio');
insert into t_instrument values('Alopeke', 'ALOPEKE', 'Alopeke');
insert into t_instrument values('Zorro', 'ZORRO', 'Zorro');
