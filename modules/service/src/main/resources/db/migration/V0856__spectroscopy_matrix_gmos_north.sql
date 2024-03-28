-- Creates the GMOS North configuration options table with GMOS north features.
-- Each row references the corresponding row in t_spectroscopy_config_option.
CREATE TABLE t_spectroscopy_config_option_gmos_north (
  c_instrument d_tag NOT NULL DEFAULT ('GmosNorth'),
  CHECK (c_instrument = 'GmosNorth'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_spectroscopy_config_option (c_instrument, c_index),

  c_fpu        d_tag NOT NULL REFERENCES t_gmos_north_fpu(c_tag),
  c_grating    d_tag NOT NULL REFERENCES t_gmos_north_disperser(c_tag),
  c_filter     d_tag          REFERENCES t_gmos_north_filter(c_tag)
);

-- Make the combined common + GMOS North specific table for bulk copy.
SELECT create_spectroscopy_config_option_temp_table(
  'gmos_north_temp',
  ARRAY[
    'c_fpu     d_tag',
    'c_grating d_tag',
    'c_filter  d_tag'
  ]
);

COPY gmos_north_temp FROM STDIN WITH (DELIMITER ',', FORMAT csv, NULL '\N', QUOTE '$');
GmosNorth,1,B1200 0.25",single_slit,0.25",250000,330000000,B1200,\N,360000,1030000,463000,164000,7488,false,\N,gn,LongSlit_0_25,B1200_G5301,\N
GmosNorth,2,B1200 0.50",single_slit,0.50",500000,330000000,B1200,\N,360000,1030000,463000,164000,3744,false,\N,gn,LongSlit_0_50,B1200_G5301,\N
GmosNorth,3,B1200 0.75",single_slit,0.75",750000,330000000,B1200,\N,360000,1030000,463000,164000,2496,false,\N,gn,LongSlit_0_75,B1200_G5301,\N
GmosNorth,4,B1200 1.0",single_slit,1.0",1000000,330000000,B1200,\N,360000,1030000,463000,164000,1872,false,\N,gn,LongSlit_1_00,B1200_G5301,\N
GmosNorth,5,B1200 1.5",single_slit,1.5",1500000,330000000,B1200,\N,360000,1030000,463000,164000,1248,false,\N,gn,LongSlit_1_50,B1200_G5301,\N
GmosNorth,6,B1200 2.0",single_slit,2.0",2000000,330000000,B1200,\N,360000,1030000,463000,164000,936,false,\N,gn,LongSlit_2_00,B1200_G5301,\N
GmosNorth,7,B1200 5.0",single_slit,5.0",5000000,330000000,B1200,\N,360000,1030000,463000,164000,374,false,\N,gn,LongSlit_5_00,B1200_G5301,\N
GmosNorth,8,B600 0.25",single_slit,0.25",250000,330000000,B600,\N,360000,1030000,461000,317000,3376,false,\N,gn,LongSlit_0_25,B600_G5307,\N
GmosNorth,9,B600 0.50",single_slit,0.50",500000,330000000,B600,\N,360000,1030000,461000,317000,1688,false,\N,gn,LongSlit_0_50,B600_G5307,\N
GmosNorth,10,B600 0.75",single_slit,0.75",750000,330000000,B600,\N,360000,1030000,461000,317000,1125,false,\N,gn,LongSlit_0_75,B600_G5307,\N
GmosNorth,11,B600 1.0",single_slit,1.0",1000000,330000000,B600,\N,360000,1030000,461000,317000,844,false,\N,gn,LongSlit_1_00,B600_G5307,\N
GmosNorth,12,B600 1.5",single_slit,1.5",1500000,330000000,B600,\N,360000,1030000,461000,317000,563,false,\N,gn,LongSlit_1_50,B600_G5307,\N
GmosNorth,13,B600 2.0",single_slit,2.0",2000000,330000000,B600,\N,360000,1030000,461000,317000,422,false,\N,gn,LongSlit_2_00,B600_G5307,\N
GmosNorth,14,B600 5.0",single_slit,5.0",5000000,330000000,B600,\N,360000,1030000,461000,317000,169,false,\N,gn,LongSlit_5_00,B600_G5307,\N
GmosNorth,15,R831 0.25",single_slit,0.25",250000,330000000,R831,\N,360000,1030000,757000,235000,8792,false,\N,gn,LongSlit_0_25,R831_G5302,\N
GmosNorth,16,R831 0.50",single_slit,0.50",500000,330000000,R831,\N,360000,1030000,757000,235000,4396,false,\N,gn,LongSlit_0_50,R831_G5302,\N
GmosNorth,17,R831 0.75",single_slit,0.75",750000,330000000,R831,\N,360000,1030000,757000,235000,2931,false,\N,gn,LongSlit_0_75,R831_G5302,\N
GmosNorth,18,R831 1.0",single_slit,1.0",1000000,330000000,R831,\N,360000,1030000,757000,235000,2198,false,\N,gn,LongSlit_1_00,R831_G5302,\N
GmosNorth,19,R831 1.5",single_slit,1.5",1500000,330000000,R831,\N,360000,1030000,757000,235000,1465,false,\N,gn,LongSlit_1_50,R831_G5302,\N
GmosNorth,20,R831 2.0",single_slit,2.0",2000000,330000000,R831,\N,360000,1030000,757000,235000,1099,false,\N,gn,LongSlit_2_00,R831_G5302,\N
GmosNorth,21,R831 5.0",single_slit,5.0",5000000,330000000,R831,\N,360000,1030000,757000,235000,440,false,\N,gn,LongSlit_5_00,R831_G5302,\N
GmosNorth,22,B480 0.25",single_slit,0.25",250000,330000000,B480,\N,360000,1030000,422000,390000,3040,false,\N,gn,LongSlit_0_25,B480_G5309,\N
GmosNorth,23,B480 0.50",single_slit,0.50",500000,330000000,B480,\N,360000,1030000,422000,390000,1520,false,\N,gn,LongSlit_0_50,B480_G5309,\N
GmosNorth,24,B480 0.75",single_slit,0.75",750000,330000000,B480,\N,360000,1030000,422000,390000,1013,false,\N,gn,LongSlit_0_75,B480_G5309,\N
GmosNorth,25,B480 1.0",single_slit,1.0",1000000,330000000,B480,\N,360000,1030000,422000,390000,760,false,\N,gn,LongSlit_1_00,B480_G5309,\N
GmosNorth,26,B480 1.5",single_slit,1.5",1500000,330000000,B480,\N,360000,1030000,422000,390000,507,false,\N,gn,LongSlit_1_50,B480_G5309,\N
GmosNorth,27,B480 2.0",single_slit,2.0",2000000,330000000,B480,\N,360000,1030000,422000,390000,380,false,\N,gn,LongSlit_2_00,B480_G5309,\N
GmosNorth,28,B480 5.0",single_slit,5.0",5000000,330000000,B480,\N,360000,1030000,422000,390000,152,false,\N,gn,LongSlit_5_00,B480_G5309,\N
GmosNorth,29,R400 0.25",single_slit,0.25",250000,330000000,R400,\N,360000,1030000,764000,472000,3836,false,\N,gn,LongSlit_0_25,R400_G5305,\N
GmosNorth,30,R400 0.50",single_slit,0.50",500000,330000000,R400,\N,360000,1030000,764000,472000,1918,false,\N,gn,LongSlit_0_50,R400_G5305,\N
GmosNorth,31,R400 0.75",single_slit,0.75",750000,330000000,R400,\N,360000,1030000,764000,472000,1279,false,\N,gn,LongSlit_0_75,R400_G5305,\N
GmosNorth,32,R400 1.0",single_slit,1.0",1000000,330000000,R400,\N,360000,1030000,764000,472000,959,false,\N,gn,LongSlit_1_00,R400_G5305,\N
GmosNorth,33,R400 1.5",single_slit,1.5",1500000,330000000,R400,\N,360000,1030000,764000,472000,639,false,\N,gn,LongSlit_1_50,R400_G5305,\N
GmosNorth,34,R400 2.0",single_slit,2.0",2000000,330000000,R400,\N,360000,1030000,764000,472000,480,false,\N,gn,LongSlit_2_00,R400_G5305,\N
GmosNorth,35,R400 5.0",single_slit,5.0",5000000,330000000,R400,\N,360000,1030000,764000,472000,192,false,\N,gn,LongSlit_5_00,R400_G5305,\N
GmosNorth,36,R150 0.25",single_slit,0.25",250000,330000000,R150,\N,360000,1030000,717000,1219000,1262,false,\N,gn,LongSlit_0_25,R150_G5308,\N
GmosNorth,37,R150 0.50",single_slit,0.50",500000,330000000,R150,\N,360000,1030000,717000,1219000,631,false,\N,gn,LongSlit_0_50,R150_G5308,\N
GmosNorth,38,R150 0.75",single_slit,0.75",750000,330000000,R150,\N,360000,1030000,717000,1219000,421,false,\N,gn,LongSlit_0_75,R150_G5308,\N
GmosNorth,39,R150 1.0",single_slit,1.0",1000000,330000000,R150,\N,360000,1030000,717000,1219000,316,false,\N,gn,LongSlit_1_00,R150_G5308,\N
GmosNorth,40,R150 1.5",single_slit,1.5",1500000,330000000,R150,\N,360000,1030000,717000,1219000,210,false,\N,gn,LongSlit_1_50,R150_G5308,\N
GmosNorth,41,R150 2.0",single_slit,2.0",2000000,330000000,R150,\N,360000,1030000,717000,1219000,158,false,\N,gn,LongSlit_2_00,R150_G5308,\N
GmosNorth,42,R150 5.0",single_slit,5.0",5000000,330000000,R150,\N,360000,1030000,717000,1219000,63,false,\N,gn,LongSlit_5_00,R150_G5308,\N
\.


-- Update the common table with the corresponding values from the temp table.
SELECT insert_into_spectroscopy_config_option('gmos_north_temp');

-- Finally update the instrument-specific table.
INSERT INTO t_spectroscopy_config_option_gmos_north (
  c_instrument,
  c_index,
  c_fpu,
  c_grating,
  c_filter
) SELECT
  c_instrument,
  c_index,
  c_fpu,
  c_grating,
  c_filter
FROM gmos_north_temp;

DROP TABLE gmos_north_temp;
