
-- Categories of planned time.
CREATE TABLE t_planned_time_category (
  c_tag         d_tag   PRIMARY KEY,
  c_name        varchar NOT NULL,
  c_description varchar NOT NULL
);

INSERT INTO t_planned_time_category VALUES(
  'config_change',
  'Config Change',
  'An estimate of the cost of configuration changes between steps'
);

INSERT INTO t_planned_time_category VALUES(
  'exposure',
  'Exposure',
  'Exposure time'
);

INSERT INTO t_planned_time_category VALUES(
  'readout',
  'Readout',
  'Detector readout cost'
);

INSERT INTO t_planned_time_category VALUES(
  'setup',
  'Setup',
  'An estimate of the cost of initial setup for an observation'
);

INSERT INTO t_planned_time_category VALUES(
  'write',
  'Write',
  'Time required to write datasets to permanent storage'
);

-- A list of things associated with a planned time estimate.  For example, if
-- the GMOS FPU changes from one step to another there is a 60 second cost
-- (though it can run in parallel and therefore be subsumed under other config
-- change costs).
CREATE TABLE t_time_estimate (
  c_tag         d_tag       PRIMARY KEY,
  c_name        varchar     NOT NULL,
  c_description varchar     NOT NULL,
  c_instrument  d_tag       NULL DEFAULT NULL REFERENCES t_instrument(c_tag),
  c_category    d_tag       NOT NULL REFERENCES t_planned_time_category(c_tag),
  c_time        interval(6) NOT NULL
);

COMMENT ON COLUMN t_time_estimate.c_instrument IS 'Associated instrument, if any.';

INSERT INTO t_time_estimate VALUES(
  'gcal_diffuser',
  'GCal Diffuser',
  'GCal diffuser change cost',
  NULL,
  'config_change',
  '5 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gcal_filter',
  'GCal Filter',
  'GCal filter change cost',
  NULL,
  'config_change',
  '5 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gcal_shutter',
  'GCal Shutter',
  'GCal shutter open/close transition cost',
  NULL,
  'config_change',
  '5 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_disperser',
  'GMOS North Disperser',
  'GMOS North disperser change cost',
  'GmosNorth',
  'config_change',
  '90 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_filter',
  'GMOS North Filter',
  'GMOS North filter change cost',
  'GmosNorth',
  'config_change',
  '20 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_fpu',
  'GMOS North FPU',
  'GMOS North FPU change cost',
  'GmosNorth',
  'config_change',
  '60 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_nod',
  'GMOS North N&S',
  'GMOS North nod and shuffle (normal offset) fixed per-cycle overhead',
  'GmosNorth',
  'config_change',
  '36 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_nod_e_offset',
  'GMOS North N&S E-Offset',
  'GMOS North nod and shuffle e-offset fixed per-cycle overhead',
  'GmosNorth',
  'config_change',
  '23.2 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_north_write',
  'GMOS North Write',
  'GMOS North write out time',
  'GmosNorth',
  'write',
  '10 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_disperser',
  'GMOS South Disperser',
  'GMOS South disperser change cost',
  'GmosSouth',
  'config_change',
  '90 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_filter',
  'GMOS South Filter',
  'GMOS South filter change cost',
  'GmosSouth',
  'config_change',
  '20 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_fpu',
  'GMOS South FPU',
  'GMOS South FPU change cost',
  'GmosSouth',
  'config_change',
  '60 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_nod',
  'GMOS South N&S',
  'GMOS South nod and shuffle (normal offset) fixed per-cycle overhead',
  'GmosSouth',
  'config_change',
  '36 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_nod_e_offset',
  'GMOS South N&S E-Offset',
  'GMOS South nod and shuffle e-offset fixed per-cycle overhead',
  'GmosSouth',
  'config_change',
  '23.2 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'gmos_south_write',
  'GMOS South Write',
  'GMOS South write out time',
  'GmosSouth',
  'write',
  '10 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'offset_constant',
  'Offset Constant',
  'Constant cost (apart from distance) for an offset',
  NULL,
  'config_change',
  '7 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'offset_distance',
  'Offset Per-Arcsec',
  'Offset cost per-arcsec of offset distance',
  NULL,
  'config_change',
  '0.006250 seconds'
);

INSERT INTO t_time_estimate VALUES(
  'science_fold',
  'Science Fold',
  'Cost for moving the science fold in or out',
  NULL,
  'config_change',
  '15 seconds'
);