
--- GMOS NORTH DETECTOR

create table t_gmos_north_detector (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name         varchar     not null,
  c_pixel_size        d_angle_µas not null,
  c_shuffle_offset    d_pixels    not null check (c_shuffle_offset > 0),
  c_x_size            d_pixels    not null check (c_x_size > 0),
  c_y_size            d_pixels    not null check (c_y_size > 0),
  c_gap_size          smallint    not null check (c_gap_size > 0),
  c_max_rois          int         not null check (c_max_rois > 0)
);
comment on table t_gmos_north_detector is 'Lookup table for GMOS North detectors.';

insert into t_gmos_north_detector values ('E2V',       'E2V',        'E2V',      72700, 1536, 6144, 4608, 37, 4);
insert into t_gmos_north_detector values ('HAMAMATSU', 'Hamamatsu', 'Hamamatsu', 80900, 1392, 6278, 4176, 80, 5);

--- GMOS NORTH STAGE MODE

create table t_gmos_north_stage_mode (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name         varchar     not null
);

insert into t_gmos_north_stage_mode values ('NoFollow',  'No Follow',  'Do Not Follow');
insert into t_gmos_north_stage_mode values ('FollowXyz', 'Follow XYZ', 'Follow in XYZ (focus)');
insert into t_gmos_north_stage_mode values ('FollowXy',  'Follow XY',  'Follow in XY');
insert into t_gmos_north_stage_mode values ('FollowZ',   'Follow Z',   'Follow in Z Only');

--- GMOS NORTH DISPERSER

create table t_gmos_north_disperser (
  c_tag                      d_tag       not null primary key,
  c_short_name               varchar     not null,
  c_long_name                varchar     not null,
  c_ruling_density           smallint    not null check (c_ruling_density > 0),
  c_dispersion_pm            smallint    not null check (c_dispersion_pm > 0),
  c_simultaneous_coverage_nm smallint    not null check (c_simultaneous_coverage_nm > 0),
  c_blaze_wavelength_nm      smallint    not null check (c_blaze_wavelength_nm > 0),
  c_reference_resolution     smallint    not null check (c_reference_resolution > 0)
);

insert into t_gmos_north_disperser values ('B1200_G5301', 'B1200', 'B1200_G5301', 1200,  26,  164, 463, 3744);
insert into t_gmos_north_disperser values ('R831_G5302',   'R831',  'R831_G5302',  831,  38,  235, 757, 4396);
insert into t_gmos_north_disperser values ('B600_G5303',   'B600',  'B600_G5303',  600,  45,  276, 461, 1688);
insert into t_gmos_north_disperser values ('B600_G5307',   'B600',  'B600_G5307',  600,  50,  317, 461, 1688);
insert into t_gmos_north_disperser values ('R600_G5304',   'R600',  'R600_G5304',  600,  52,  328, 926, 3744);
insert into t_gmos_north_disperser values ('B480_G5309',   'B480',  'B480_G5309',  480,  62,  390, 422, 1520);
insert into t_gmos_north_disperser values ('R400_G5305',   'R400',  'R400_G5305',  400,  74,  472, 764, 1918);
insert into t_gmos_north_disperser values ('R150_G5306',   'R150',  'R150_G5306',  150, 174, 1071, 717,  631);
insert into t_gmos_north_disperser values ('R150_G5308',   'R150',  'R150_G5308',  150, 193, 1219, 717,  631);

--- GMOS NORTH FILTER

create table t_gmos_north_filter (
  c_tag               d_tag           not null primary key,
  c_short_name        varchar         not null,
  c_long_name         varchar         not null,
  c_wavelength        d_wavelength_pm not null,
  d_width             d_wavelength_pm_range, -- can be null
  d_filter_type       d_tag           not null references t_filter_type(c_tag)
);

insert into t_gmos_north_filter values ('GPrime', 'g', 'g_G0301', 475000, '[398000, 552000]', 'BroadBand');
insert into t_gmos_north_filter values ('RPrime', 'r', 'r_G0303', 630000, '[562000, 698000]', 'BroadBand');
insert into t_gmos_north_filter values ('IPrime', 'i', 'i_G0302', 780000, '[706000, 850000]', 'BroadBand');
insert into t_gmos_north_filter values ('ZPrime', 'z', 'z_G0304', 925000, null, 'BroadBand');
insert into t_gmos_north_filter values ('Z', 'Z', 'Z_G0322', 876000, '[830000, 925000]', 'BroadBand');
insert into t_gmos_north_filter values ('Y', 'Y', 'Y_G0323', 1010000, '[970000, 1010000]', 'BroadBand');
insert into t_gmos_north_filter values ('GG455', 'GG455', 'GG455_G0305', 680000, null, 'Spectroscopic');
insert into t_gmos_north_filter values ('OG515', 'OG515', 'OG515_G0306', 710000, null, 'Spectroscopic');
insert into t_gmos_north_filter values ('RG610', 'RG610', 'RG610_G0307', 750000, null, 'Spectroscopic');
insert into t_gmos_north_filter values ('CaT', 'CaT', 'CaT_G0309', 860000, '[780000, 933000]', 'BroadBand');
insert into t_gmos_north_filter values ('Ha', 'Ha', 'Ha_G0310', 656000, '[654000, 661000]', 'NarrowBand');
insert into t_gmos_north_filter values ('HaC', 'HaC', 'HaC_G0311', 662000, '[659000, 665000]', 'NarrowBand');
insert into t_gmos_north_filter values ('DS920', 'DS920', 'DS920_G0312', 920000, '[912800, 931400]', 'NarrowBand');
insert into t_gmos_north_filter values ('SII', 'SII', 'SII_G0317', 672000, '[669400, 673700]', 'NarrowBand');
insert into t_gmos_north_filter values ('OIII', 'OIII', 'OIII_G0318', 499000, '[496500, 501500]', 'NarrowBand');
insert into t_gmos_north_filter values ('OIIIC', 'OIIIC', 'OIIIC_G0319', 514000, '[509000, 519000]', 'NarrowBand');
insert into t_gmos_north_filter values ('HeII', 'HeII', 'HeII_G0320', 468000, '[464000, 472000]', 'NarrowBand');
insert into t_gmos_north_filter values ('HeIIC', 'HeIIC', 'HeIIC_G0321', 478000, '[474000, 482000]', 'NarrowBand');
insert into t_gmos_north_filter values ('HartmannA_RPrime', 'r+HartA', 'HartmannA_G0313 + r_G0303', 630000, null, 'Engineering');
insert into t_gmos_north_filter values ('HartmannB_RPrime', 'r+HartB', 'HartmannB_G0314 + r_G0303', 630000, null, 'Engineering');
insert into t_gmos_north_filter values ('GPrime_GG455', 'g+GG455', 'g_G0301 + GG455_G0305', 506000, '[460000, 552000]', 'Combination');
insert into t_gmos_north_filter values ('GPrime_OG515', 'g+OG515', 'g_G0301 + OG515_G0306', 536000, '[520000, 552000]', 'Combination');
insert into t_gmos_north_filter values ('RPrime_RG610', 'r+RG610', 'r_G0303 + RG610_G0307', 657000, '[615000, 698000]', 'Combination');
insert into t_gmos_north_filter values ('IPrime_CaT', 'i+CaT', 'i_G0302 + CaT_G0309', 815000, '[780000, 850000]', 'Combination');
insert into t_gmos_north_filter values ('ZPrime_CaT', 'z+CaT', 'z_G0305 + CaT_G0309', 890000, '[848000, 933000]', 'Combination');
insert into t_gmos_north_filter values ('UPrime', 'u', 'u_G0308', 350000, '[336000, 385000]', 'BroadBand');

--- GMOS NORTH FPU

create table t_gmos_north_fpu (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null,
  c_slit_width        d_angle_µas,
  c_x_offset          d_angle_µas not null
);

insert into t_gmos_north_fpu values ('Ns0', 'NS0.25ʺ', 'N and S 0.25 arcsec', 250000, 0);
insert into t_gmos_north_fpu values ('Ns1', 'NS0.5ʺ', 'N and S 0.50 arcsec', 500000, 0);
insert into t_gmos_north_fpu values ('Ns2', 'NS0.75ʺ', 'N and S 0.75 arcsec', 750000, 0);
insert into t_gmos_north_fpu values ('Ns3', 'NS1.0ʺ', 'N and S 1.00 arcsec', 1000000, 0);
insert into t_gmos_north_fpu values ('Ns4', 'NS1.5ʺ', 'N and S 1.50 arcsec', 1500000, 0);
insert into t_gmos_north_fpu values ('Ns5', 'NS2.0ʺ', 'N and S 2.00 arcsec', 2000000, 0);
insert into t_gmos_north_fpu values ('LongSlit_0_25', '0.25ʺ', 'Longslit 0.25 arcsec', 250000, 0);
insert into t_gmos_north_fpu values ('LongSlit_0_50', '0.50ʺ', 'Longslit 0.50 arcsec', 500000, 0);
insert into t_gmos_north_fpu values ('LongSlit_0_75', '0.75ʺ', 'Longslit 0.75 arcsec', 750000, 0);
insert into t_gmos_north_fpu values ('LongSlit_1_00', '1.0ʺ', 'Longslit 1.00 arcsec', 1000000, 0);
insert into t_gmos_north_fpu values ('LongSlit_1_50', '1.5ʺ', 'Longslit 1.50 arcsec', 1500000, 0);
insert into t_gmos_north_fpu values ('LongSlit_2_00', '2.0ʺ', 'Longslit 2.00 arcsec', 2000000, 0);
insert into t_gmos_north_fpu values ('LongSlit_5_00', '5.0ʺ', 'Longslit 5.00 arcsec', 5000000, 0);
insert into t_gmos_north_fpu values ('Ifu1', 'IFU-2', 'IFU 2 Slits', null, 33500000);
insert into t_gmos_north_fpu values ('Ifu2', 'IFU-B', 'IFU Left Slit (blue)', null, 31750000);
insert into t_gmos_north_fpu values ('Ifu3', 'IFU-R', 'IFU Right Slit (red)', null, 35250000);

--- GMOS NORTH STATIC CONFIG

create table t_gmos_north_static (

  c_observation_id d_observation_id NOT NULL PRIMARY KEY,
  c_instrument     d_tag            NOT NULL DEFAULT ('GmosNorth'),

  -- We link back to the observation table thus.
  FOREIGN KEY (c_observation_id, c_instrument)
  REFERENCES t_observation (c_observation_id, c_instrument),
  CHECK (c_instrument = 'GmosNorth'),

  c_detector          d_tag not null REFERENCES t_gmos_north_detector(c_tag),
  c_mos_pre_imaging   boolean not null,
  c_nod_and_shuffle   bigint references t_gmos_nod_and_shuffle(c_gmos_nod_and_shuffle_id),
  c_stage_mode        d_tag not null references t_gmos_north_stage_mode(c_tag)

);


--- GMOS NORTH DYNAMIC CONFIG

create type e_gmos_fpu_option as enum ('None', 'CustomMask', 'FPU');
create table t_gmos_north_dynamic (

  c_observation_id d_observation_id NOT NULL PRIMARY KEY,
  c_instrument d_tag            NOT NULL DEFAULT ('GmosNorth'),

  -- We link back to the observation table thus.
  FOREIGN KEY (c_observation_id, c_instrument)
  REFERENCES t_observation (c_observation_id, c_instrument),
  CHECK (c_instrument = 'GmosNorth'),

  -- These elements are ordered
  c_index         smallint         NOT NULL,
  UNIQUE (c_observation_id, c_index),

  c_exposure               interval not null,

  c_readout_x_binning      d_tag not null references t_gmos_binning(c_tag),
  c_readout_y_binning      d_tag not null references t_gmos_binning(c_tag),
  c_readout_amp_count      d_tag not null references t_gmos_amp_count(c_tag),
  c_readout_amp_gain       d_tag not null references t_gmos_amp_gain(c_tag),
  c_readout_amp_read_mode  d_tag not null references t_gmos_amp_read_mode(c_tag),

  c_dtax                   d_tag not null references t_gmos_dtax(c_tag),
  c_roi                    d_tag not null references t_gmos_roi(c_tag),

  -- all null or all non-null
  c_grating_disperser  d_tag references t_gmos_north_disperser(c_tag),
  c_grating_order      d_tag references t_gmos_disperser_order(c_tag),
  c_grating_wavelength d_wavelength_pm,
  check (
    (
      c_grating_disperser  is null and
      c_grating_order      is null and
      c_grating_wavelength is null
    ) or (
      c_grating_disperser  is not null and
      c_grating_order      is not null and
      c_grating_wavelength is not null
    )
  ),

  c_filter d_tag references t_gmos_north_filter(c_tag),

  -- discriminant with three choices
  c_fpu_option                        e_gmos_fpu_option not null,
  c_fpu_custom_mask_filename          varchar           check(length(c_fpu_custom_mask_filename) > 0),
  c_fpu_custom_mask_custom_slit_width d_tag             references t_gmos_custom_slit_width(c_tag),
  c_fpu_fpu                           d_tag             references t_gmos_north_fpu(c_tag),
  check (
    (
      c_fpu_option = 'None' and
      c_fpu_custom_mask_filename is null and
      c_fpu_custom_mask_custom_slit_width is null and
      c_fpu_fpu is null
    ) or (
      c_fpu_option = 'CustomMask' and
      c_fpu_custom_mask_filename is NOT null and
      c_fpu_custom_mask_custom_slit_width is NOT null and
      c_fpu_fpu is null
    ) or (
      c_fpu_option = 'FPU' and
      c_fpu_custom_mask_filename is null and
      c_fpu_custom_mask_custom_slit_width is null and
      c_fpu_fpu is NOT null
    )
  )

);

--- GMOS NORTH LONG SLIT OBSERVING MODE

create table t_gmos_north_long_slit (

  c_observation_id             d_observation_id      NOT NULL,
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'gmos_north_long_slit' check (c_observing_mode_type = 'gmos_north_long_slit'),

  c_grating                    d_tag                 NOT NULL             REFERENCES t_gmos_north_disperser(c_tag),
  c_filter                     d_tag                 NULL DEFAULT NULL    REFERENCES t_gmos_north_filter(c_tag),
  c_fpu                        d_tag                 NOT NULL             REFERENCES t_gmos_north_fpu(c_tag),
  c_central_wavelength         d_wavelength_pm       NULL DEFAULT NULL,

  c_xbin                       d_tag                 NULL DEFAULT NULL   REFERENCES t_gmos_binning(c_tag),
  c_ybin                       d_tag                 NULL DEFAULT NULL   REFERENCES t_gmos_binning(c_tag),
  c_amp_read_mode              d_tag                 NULL DEFAULT NULL   REFERENCES t_gmos_amp_read_mode(c_tag),
  c_amp_gain                   d_tag                 NULL DEFAULT NULL   REFERENCES t_gmos_amp_gain(c_tag),
  c_roi                        d_tag                 NULL DEFAULT NULL   REFERENCES t_gmos_roi(c_tag),

  -- stuff wavelength dithers and offsets into a string until grackle supports array columns?
  c_wavelength_dithers         text                  NULL DEFAULT NULL,
  c_spatial_offsets            text                  NULL DEFAULT NULL,

  -- hold on to the initial grating, filter, fpu and central wavelength regardless of subsequent changes
  c_initial_grating            d_tag                 NOT NULL             REFERENCES t_gmos_north_disperser(c_tag),
  c_initial_filter             d_tag                 NULL DEFAULT NULL    REFERENCES t_gmos_north_filter(c_tag),
  c_initial_fpu                d_tag                 NOT NULL             REFERENCES t_gmos_north_fpu(c_tag),
  c_initial_central_wavelength d_wavelength_pm       NULL DEFAULT NULL,

  CONSTRAINT wavelength_dither_format CHECK (c_wavelength_dithers ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),
  CONSTRAINT offset_format            CHECK (c_spatial_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),

  PRIMARY KEY (c_observation_id, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

