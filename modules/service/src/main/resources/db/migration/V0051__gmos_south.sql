
--- GMOS SOUTH DETECTOR

create table t_gmos_south_detector (
  c_tag            d_tag       not null primary key,
  c_short_name     varchar     not null,
  c_long_name      varchar     not null,
  c_pixel_size     d_angle_µas not null,
  c_shuffle_offset d_pixels    not null check (c_shuffle_offset > 0),
  c_x_size         d_pixels    not null check (c_x_size > 0),
  c_y_size         d_pixels    not null check (c_y_size > 0),
  c_gap_size       smallint    not null check (c_gap_size > 0),
  c_max_rois       int         not null check (c_max_rois > 0)
);
comment on table t_gmos_south_detector is 'Lookup table for GMOS South detectors.';

insert into t_gmos_south_detector values ('E2V',       'E2V',       'E2V',       73000, 1536, 6144, 4608, 37, 4);
insert into t_gmos_south_detector values ('HAMAMATSU', 'Hamamatsu', 'Hamamatsu', 80000, 1392, 6255, 4176, 61, 5);

--- GMOS SOUTH STAGE MODE

create table t_gmos_south_stage_mode (
  c_tag        d_tag       not null primary key,
  c_short_name varchar     not null,
  c_long_name  varchar     not null
);

insert into t_gmos_south_stage_mode values ('NoFollow',  'No Follow',  'Do Not Follow');
insert into t_gmos_south_stage_mode values ('FollowXyz', 'Follow XYZ', 'Follow in XYZ (focus)');
insert into t_gmos_south_stage_mode values ('FollowXy',  'Follow XY',  'Follow in XY');
insert into t_gmos_south_stage_mode values ('FollowZ',   'Follow Z',   'Follow in Z Only');

--- GMOS SOUTH DISPERSER

create table t_gmos_south_disperser (
  c_tag                      d_tag       not null primary key,
  c_short_name               varchar     not null,
  c_long_name                varchar     not null,
  c_ruling_density           smallint    not null check (c_ruling_density > 0),
  c_dispersion_pm            smallint    not null check (c_dispersion_pm  > 0),
  c_simultaneous_coverage_nm smallint    not null check (c_simultaneous_coverage_nm > 0),
  c_blaze_wavelength_nm      smallint    not null check (c_blaze_wavelength_nm > 0),
  c_reference_resolution     smallint    not null check (c_reference_resolution > 0)
);

insert into t_gmos_south_disperser values ('B1200_G5321', 'B1200', 'B1200_G5321', 1200,  26,  159, 463, 3744);
insert into t_gmos_south_disperser values ('R831_G5322',  'R831',  'R831_G5322',   831,  38,  230, 757, 4396);
insert into t_gmos_south_disperser values ('B600_G5323',  'B600',  'B600_G5323',   600,  50,  307, 461, 1688);
insert into t_gmos_south_disperser values ('B600_G5324',  'B600',  'B600_G5324',   600,  52,  318, 926, 3744);
insert into t_gmos_south_disperser values ('B480_G5327',  'B480',  'B480_G5327',   480,  62,  390, 422, 1520);
insert into t_gmos_south_disperser values ('R400_G5325',  'R400',  'R400_G5325',   400,  74,  462, 764, 1918);
insert into t_gmos_south_disperser values ('R150_G5326',  'R150',  'R150_G5326',   150, 193, 1190, 717,  631);


--- GMOS SOUTH FILTER

create table t_gmos_south_filter (
  c_tag         d_tag           not null primary key,
  c_short_name  varchar         not null,
  c_long_name 	varchar         not null,
  c_wavelength  d_wavelength_pm not null,
  d_width       d_wavelength_pm_range, -- can be null
  d_filter_type d_tag           not null references t_filter_type(c_tag)
);

insert into t_gmos_south_filter values ('UPrime', 'u', 'u_G0332', 350000, '[336000, 385000]', 'BroadBand');
insert into t_gmos_south_filter values ('GPrime', 'g', 'g_G0325', 475000, '[398000, 552000]', 'BroadBand');
insert into t_gmos_south_filter values ('RPrime', 'r', 'r_G0326', 630000, '[562000, 698000]', 'BroadBand');
insert into t_gmos_south_filter values ('IPrime', 'i', 'i_G0327', 780000, '[706000, 850000]', 'BroadBand');
insert into t_gmos_south_filter values ('ZPrime', 'z', 'z_G0328', 925000, null, 'BroadBand');
insert into t_gmos_south_filter values ('Z', 'Z', 'Z_G0343', 876000, '[830000, 925000]', 'BroadBand');
insert into t_gmos_south_filter values ('Y', 'Y', 'Y_G0344', 1010000, '[970000, 1010000]', 'BroadBand');
insert into t_gmos_south_filter values ('GG455', 'GG455', 'GG455_G0329', 680000, null, 'Spectroscopic');
insert into t_gmos_south_filter values ('OG515', 'OG515', 'OG515_G0330', 710000, null, 'Spectroscopic');
insert into t_gmos_south_filter values ('RG610', 'RG610', 'RG610_G0331', 750000, null, 'Spectroscopic');
insert into t_gmos_south_filter values ('RG780', 'RG780', 'RG780_G0334', 850000, null, 'Spectroscopic');
insert into t_gmos_south_filter values ('CaT', 'CaT', 'CaT_G0333', 860000, '[780000, 933000]', 'BroadBand');
insert into t_gmos_south_filter values ('HartmannA_RPrime', 'r+HartA', 'HartmannA_G0337 + r_G0326', 630000, null, 'Engineering');
insert into t_gmos_south_filter values ('HartmannB_RPrime', 'r+HartB', 'HartmannB_G0338 + r_G0326', 630000, null, 'Engineering');
insert into t_gmos_south_filter values ('GPrime_GG455', 'g+GG455', 'g_G0325 + GG455_G0329', 506000, '[460000, 552000]', 'Combination');
insert into t_gmos_south_filter values ('GPrime_OG515', 'g+OG515', 'g_G0325 + OG515_G0330', 536000, '[520000, 552000]', 'Combination');
insert into t_gmos_south_filter values ('RPrime_RG610', 'r+RG610', 'r_G0326 + RG610_G0331', 657000, '[615000, 698000]', 'Combination');
insert into t_gmos_south_filter values ('IPrime_RG780', 'i+RG780', 'i_G0327 + RG780_G0334', 819000, '[777000, 851000]', 'Combination');
insert into t_gmos_south_filter values ('IPrime_CaT', 'i+CaT', 'i_G0327 + CaT_G0333', 815000, '[780000, 850000]', 'Combination');
insert into t_gmos_south_filter values ('ZPrime_CaT', 'z+Cat', 'z_G0328 + CaT_G0333', 890000, '[848000, 933000]', 'Combination');
insert into t_gmos_south_filter values ('Ha', 'Ha', 'Ha_G0336', 656000, '[654000, 661000]', 'NarrowBand');
insert into t_gmos_south_filter values ('SII', 'SII', 'SII_G0335', 672000, '[669400, 673700]', 'NarrowBand');
insert into t_gmos_south_filter values ('HaC', 'HaC', 'HaC_G0337', 662000, '[659000, 665000]', 'NarrowBand');
insert into t_gmos_south_filter values ('OIII', 'OIII', 'OIII_G0338', 499000, '[496500, 501500]', 'NarrowBand');
insert into t_gmos_south_filter values ('OIIIC', 'OIIIC', 'OIIIC_G0339', 514000, '[509000, 519000]', 'NarrowBand');
insert into t_gmos_south_filter values ('HeII', 'HeII', 'HeII_G0340', 468000, '[464000, 472000]', 'NarrowBand');
insert into t_gmos_south_filter values ('HeIIC', 'HeIIC', 'HeIIC_G0341', 478000, '[474000, 482000]', 'NarrowBand');
insert into t_gmos_south_filter values ('Lya395', 'Lya395', 'Lya395_G0342', 396000, null, 'NarrowBand');

--- GMOS SOUTH FPU

create table t_gmos_south_fpu (
  c_tag        d_tag        not null primary key,
  c_short_name varchar      not null,
  c_long_name  varchar      not null,
  c_slit_width d_angle_µas,
  c_x_offset   d_angle_µas  not null
);

insert into t_gmos_south_fpu values ('Bhros',         'bHROS',    'bHROS',                              0,         0);
insert into t_gmos_south_fpu values ('Ns1',           'NS0.5\"',  'N and S 0.50 arcsec',           500000,         0);
insert into t_gmos_south_fpu values ('Ns2',           'NS0.75\"', 'N and S 0.75 arcsec',           750000,         0);
insert into t_gmos_south_fpu values ('Ns3',           'NS1.0\"',  'N and S 1.00 arcsec',          1000000,         0);
insert into t_gmos_south_fpu values ('Ns4',           'NS1.5\"',  'N and S 1.50 arcsec',          1500000,         0);
insert into t_gmos_south_fpu values ('Ns5',           'NS2.0\"',  'N and S 2.00 arcsec',          2000000,         0);
insert into t_gmos_south_fpu values ('LongSlit_0_25', '0.25\"',   'Longslit 0.25 arcsec',          250000,         0);
insert into t_gmos_south_fpu values ('LongSlit_0_50', '0.50\"',   'Longslit 0.50 arcsec',          500000,         0);
insert into t_gmos_south_fpu values ('LongSlit_0_75', '0.75\"',   'Longslit 0.75 arcsec',          750000,         0);
insert into t_gmos_south_fpu values ('LongSlit_1_00', '1.0\"',    'Longslit 1.00 arcsec',         1000000,         0);
insert into t_gmos_south_fpu values ('LongSlit_1_50', '1.5\"',    'Longslit 1.50 arcsec',         1500000,         0);
insert into t_gmos_south_fpu values ('LongSlit_2_00', '2.0\"',    'Longslit 2.00 arcsec',         2000000,         0);
insert into t_gmos_south_fpu values ('LongSlit_5_00', '5.0\"',    'Longslit 5.00 arcsec',         5000000,         0);
insert into t_gmos_south_fpu values ('Ifu2Slits',     'IFU-2',    'IFU 2 Slits',                   310000, -31750000);
insert into t_gmos_south_fpu values ('IfuBlue',       'IFU-B',    'IFU Left Slit (blue)',          310000, -30875000);
insert into t_gmos_south_fpu values ('IfuRed',        'IFU-R', '   IFU Right Slit (red)',          310000, -32625000);
insert into t_gmos_south_fpu values ('IfuNS2Slits',   'IFU-NS-2', 'IFU N and S 2 Slits',           310000, -31750000);
insert into t_gmos_south_fpu values ('IfuNSBlue',     'IFU-NS-B', 'IFU N and S Left Slit (blue)',  310000, -30875000);
insert into t_gmos_south_fpu values ('IfuNSRed',      'IFU-NS-R', 'IFU N and S Right Slit (red)',  310000, -32625000);