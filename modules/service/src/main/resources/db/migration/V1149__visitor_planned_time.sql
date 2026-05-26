-- Resident visitor instrument time estimates:
-- acquisition (setup) and per-exposure readout overheads for
-- Alopeke, Zorro, and MAROON-X.
--
-- Total time is calculated as:
--   setup + sum(over science ETMs) count * (exposureTime + readout)

INSERT INTO t_time_estimate VALUES(
  'alopeke_speckle_setup',
  'Alopeke Speckle Setup',
  'Alopeke speckle mode acquisition cost',
  'Alopeke',
  '5 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'alopeke_speckle_readout',
  'Alopeke Speckle Readout',
  'Alopeke speckle per-exposure readout cost',
  'Alopeke',
  '4 milliseconds'
);

INSERT INTO t_time_estimate VALUES(
  'alopeke_wide_field_setup',
  'Alopeke Wide Field Setup',
  'Alopeke wide field mode acquisition cost',
  'Alopeke',
  '6 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'alopeke_wide_field_readout',
  'Alopeke Wide Field Readout',
  'Alopeke wide field per-exposure readout cost',
  'Alopeke',
  '5 milliseconds'
);

INSERT INTO t_time_estimate VALUES(
  'zorro_speckle_setup',
  'Zorro Speckle Setup',
  'Zorro speckle mode acquisition cost',
  'Zorro',
  '5 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'zorro_speckle_readout',
  'Zorro Speckle Readout',
  'Zorro speckle per-exposure readout cost',
  'Zorro',
  '4 milliseconds'
);

INSERT INTO t_time_estimate VALUES(
  'zorro_wide_field_setup',
  'Zorro Wide Field Setup',
  'Zorro wide field mode acquisition cost',
  'Zorro',
  '6 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'zorro_wide_field_readout',
  'Zorro Wide Field Readout',
  'Zorro wide field per-exposure readout cost',
  'Zorro',
  '5 milliseconds'
);

INSERT INTO t_time_estimate VALUES(
  'maroon_x_setup',
  'MAROON-X Setup',
  'MAROON-X acquisition cost',
  'MaroonX',
  '5 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'maroon_x_readout',
  'MAROON-X Readout',
  'MAROON-X per-exposure readout cost',
  'MaroonX',
  '100 seconds'
);
