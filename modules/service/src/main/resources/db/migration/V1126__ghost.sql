-- GHOST readout times

CREATE TABLE t_ghost_readout (
  c_id        SERIAL      PRIMARY KEY,
  c_read_mode d_tag       NOT NULL REFERENCES t_ghost_read_mode(c_tag),
  c_binning   d_tag       NOT NULL REFERENCES t_ghost_binning(c_tag),
  c_red_time  interval    NOT NULL,
  c_blue_time interval    NOT NULL
);

-- https://www.gemini.edu/instrumentation/ghost/components Table 3
COPY t_ghost_readout (
  c_read_mode,
  c_binning,
  c_red_time,
  c_blue_time
) FROM STDIN;
slow	one_by_one	97.4 seconds	45.6 seconds
slow	one_by_two	49.6 seconds	24.8 seconds
slow	one_by_four	25.7 seconds	14.4 seconds
slow	one_by_eight	13.8 seconds	9.1 seconds
slow	two_by_two	27.5 seconds	15.4 seconds
slow	two_by_four	14.7 seconds	9.8 seconds
slow	two_by_eight	8.4 seconds	7.0 seconds
slow	four_by_four	9.5 seconds	7.9 seconds
medium	one_by_one	50.1 seconds	24.6 seconds
medium	one_by_two	26.1 seconds	14.3 seconds
medium	one_by_four	13.9 seconds	9.1 seconds
medium	one_by_eight	7.9 seconds	6.5 seconds
medium	two_by_two	15.7 seconds	10.1 seconds
medium	two_by_four	8.8 seconds	7.2 seconds
medium	two_by_eight	5.4 seconds	5.6 seconds
medium	four_by_four	6.5 seconds	6.5 seconds
fast	one_by_one	21.7 seconds	12.0 seconds
fast	one_by_two	11.7 seconds	7.9 seconds
fast	one_by_four	6.8 seconds	5.9 seconds
fast	one_by_eight	4.3 seconds	4.9 seconds
fast	two_by_two	8.6 seconds	6.9 seconds
fast	two_by_four	5.2 seconds	5.6 seconds
fast	two_by_eight	3.6 seconds	4.9 seconds
fast	four_by_four	4.7 seconds	5.8 seconds
\.

INSERT INTO t_time_estimate VALUES(
  'ghost_ifu_max_visit',
  'GHOST IFU Max Visit',
  'GHOST IFU Max Visit',
  'Ghost',
  '2 hours'
);

INSERT INTO t_time_estimate VALUES(
  'ghost_ifu_setup',
  'GHOST IFU Setup',
  'GHOST IFU full setup cost',
  'Ghost',
  '8 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'ghost_reacquisition',
  'GHOST Reacquisition',
  'GHOST reacquisition cost',
  'Ghost',
  '5 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'ghost_write',
  'GHOST Write',
  'GHOST write out time',
  'Ghost',
  '5 seconds'
);
