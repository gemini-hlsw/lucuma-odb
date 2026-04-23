-- GHOST readout times

CREATE TABLE t_ghost_readout (
  c_id        SERIAL      PRIMARY KEY,
  c_read_mode d_tag       NOT NULL REFERENCES t_ghost_read_mode(c_tag),
  c_binning   d_tag       NOT NULL REFERENCES t_ghost_binning(c_tag),
  c_red_time  interval    NOT NULL,
  c_blue_time interval    NOT NULL
);

-- Taken from:
-- https://github.com/gemini-hlsw/seqexec/blob/main/modules/server/src/main/scala/seqexec/server/ghost/GhostLUT.scala#L145

-- The values published at:
-- https://www.gemini.edu/instrumentation/ghost/components (Table 3)
-- are reportedly fake news.

COPY t_ghost_readout (
  c_read_mode,
  c_binning,
  c_red_time,
  c_blue_time
) FROM STDIN;
slow	one_by_one	100.675 seconds	45.957 seconds
slow	one_by_two	51.271 seconds	23.808 seconds
slow	one_by_four	26.564 seconds	12.741 seconds
slow	one_by_eight	14.198 seconds	7.229 seconds
slow	two_by_two	28.364 seconds	13.644 seconds
slow	two_by_four	15.146 seconds	7.68 seconds
slow	two_by_eight	8.534 seconds	4.722 seconds
slow	four_by_four	9.536 seconds	5.226 seconds
medium	one_by_one	58.994 seconds	27.118 seconds
medium	one_by_two	30.230 seconds	14.237 seconds
medium	one_by_four	15.838 seconds	7.784 seconds
medium	one_by_eight	8.686 seconds	4.574 seconds
medium	two_by_two	17.696 seconds	8.633 seconds
medium	two_by_four	9.638 seconds	5.24 seconds
medium	two_by_eight	5.580 seconds	3.223 seconds
medium	four_by_four	6.581 seconds	3.722 seconds
fast	one_by_one	23.520 seconds	11.78 seconds
fast	one_by_two	12.341 seconds	6.72 seconds
fast	one_by_four	6.773 seconds	3.575 seconds
fast	one_by_eight	3.977 seconds	3.75 seconds
fast	two_by_two	8.577 seconds	4.425 seconds
fast	two_by_four	4.929 seconds	3.71 seconds
fast	two_by_eight	3.578 seconds	3.42 seconds
fast	four_by_four	4.77 seconds	3.44 seconds
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
