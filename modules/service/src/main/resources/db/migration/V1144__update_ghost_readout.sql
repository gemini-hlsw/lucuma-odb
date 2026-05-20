-- Updated GHOST readout times

TRUNCATE t_ghost_readout;

COPY t_ghost_readout (
  c_read_mode,
  c_binning,
  c_red_time,
  c_blue_time
) FROM STDIN;
slow	one_by_one	100.662 seconds	45.948 seconds
slow	one_by_two	51.261 seconds	23.804 seconds
slow	one_by_four	26.558 seconds	12.743 seconds
slow	one_by_eight	14.192 seconds	7.227 seconds
slow	two_by_two	28.363 seconds	13.642 seconds
slow	two_by_four	15.148 seconds	7.68 seconds
slow	two_by_eight	8.534 seconds	4.722 seconds
slow	four_by_four	9.537 seconds	5.225 seconds
medium	one_by_one	58.985 seconds	27.117 seconds
medium	one_by_two	30.218 seconds	14.235 seconds
medium	one_by_four	15.84 seconds	7.778 seconds
medium	one_by_eight	8.683 seconds	4.576 seconds
medium	two_by_two	17.691 seconds	8.633 seconds
medium	two_by_four	9.636 seconds	5.026 seconds
medium	two_by_eight	5.577 seconds	3.223 seconds
medium	four_by_four	6.579 seconds	3.724 seconds
fast	one_by_one	23.514 seconds	11.083 seconds
fast	one_by_two	12.342 seconds	6.068 seconds
fast	one_by_four	6.771 seconds	3.575 seconds
fast	one_by_eight	3.981 seconds	3.072 seconds
fast	two_by_two	8.575 seconds	4.425 seconds
fast	two_by_four	4.928 seconds	3.074 seconds
fast	two_by_eight	3.576 seconds	3.047 seconds
fast	four_by_four	4.075 seconds	3.046 seconds
\.
