
--- GMOS NOD AND SHUFFLE

create table t_gmos_nod_and_shuffle (
  c_gmos_nod_and_shuffle_id bigserial primary key,
  c_pos_A                   d_offset not null,
  c_pod_B                   d_offset not null,
  c_electronic_offsetting   boolean  not null,
  c_shuffle_offset          int      not null,
  c_shuffle_cucles          int      not null
);

--- GMOS BINNING

create table t_gmos_binning (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null,
  c_count             smallint    not null check(c_count > 0)
);

insert into t_gmos_binning values ('One', '1', 'One', 1);
insert into t_gmos_binning values ('Two', '2', 'Two', 2);
insert into t_gmos_binning values ('Four', '4', 'Four', 4);

--- GMOS AMP COUNT

create table t_gmos_amp_count (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null,
  c_count             smallint    not null check(c_count > 0)
);

insert into t_gmos_amp_count values ('Three', 'Three', 'Three', 3);
insert into t_gmos_amp_count values ('Six', 'Six', 'Six', 6);
insert into t_gmos_amp_count values ('Twelve', 'Twelve', 'Twelve', 12);

--- GMOS AMP GAIN

create table t_gmos_amp_gain (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null
);

insert into t_gmos_amp_gain values ('Low', 'Low', 'Low');
insert into t_gmos_amp_gain values ('High', 'High', 'High');

--- GMOS AMP READ MODE

create table t_gmos_amp_read_mode (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null
);

insert into t_gmos_amp_read_mode values ('Slow', 'Slow', 'Slow');
insert into t_gmos_amp_read_mode values ('Fast', 'Fast', 'Fast');

--- GMOS DTAX

create table t_gmos_dtax (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null,
  c_dtax              smallint    not null
);

insert into t_gmos_dtax values ('MinusSix', '-6', '-6', -6);
insert into t_gmos_dtax values ('MinusFive', '-5', '-5', -5);
insert into t_gmos_dtax values ('MinusFour', '-4', '-4', -4);
insert into t_gmos_dtax values ('MinusThree', '-3', '-3', -3);
insert into t_gmos_dtax values ('MinusTwo', '-2', '-2', -2);
insert into t_gmos_dtax values ('MinusOne', '-1', '-1', -1);
insert into t_gmos_dtax values ('Zero', '0', '0', 0);
insert into t_gmos_dtax values ('One', '1', '1', 1);
insert into t_gmos_dtax values ('Two', '2', '2', 2);
insert into t_gmos_dtax values ('Three', '3', '3', 3);
insert into t_gmos_dtax values ('Four', '4', '4', 4);
insert into t_gmos_dtax values ('Five', '5', '5', 5);
insert into t_gmos_dtax values ('Six', '6', '6', 6);

--- GMOS ROI

create table t_gmos_roi (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null
);

insert into t_gmos_roi values ('FullFrame', 'full', 'Full Frame Readout');
insert into t_gmos_roi values ('Ccd2', 'ccd2', 'CCD 2');
insert into t_gmos_roi values ('CentralSpectrum', 'cspec', 'Central Spectrum');
insert into t_gmos_roi values ('CentralStamp', 'stamp', 'Central Stamp');
insert into t_gmos_roi values ('TopSpectrum', 'tspec', 'Top Spectrum');
insert into t_gmos_roi values ('BottomSpectrum', 'bspec', 'Bottom Spectrum');
insert into t_gmos_roi values ('Custom', 'custom', 'Custom ROI');

--- GMOS DISPERSER ORDER

create table t_gmos_disperser_order (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null,
  count               smallint    not null
);

insert into t_gmos_disperser_order values ('Zero', '0', 'Zero', 0);
insert into t_gmos_disperser_order values ('One', '1', 'One', 1);
insert into t_gmos_disperser_order values ('Two', '2', 'Two', 2);

--- GMOS CUSTOM SLIT WIDTH

create table t_gmos_custom_slit_width (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null,
  c_width             d_angle_µas not null
);

insert into t_gmos_custom_slit_width values ('CustomWidth_0_25', '0.25ʺ', '0.25 arcsec', 250000);
insert into t_gmos_custom_slit_width values ('CustomWidth_0_50', '0.50ʺ', '0.50 arcsec', 500000);
insert into t_gmos_custom_slit_width values ('CustomWidth_0_75', '0.75ʺ', '0.75 arcsec', 750000);
insert into t_gmos_custom_slit_width values ('CustomWidth_1_00', '1.00ʺ', '1.00 arcsec', 1000000);
insert into t_gmos_custom_slit_width values ('CustomWidth_1_50', '1.50ʺ', '1.50 arcsec', 1500000);
insert into t_gmos_custom_slit_width values ('CustomWidth_2_00', '2.00ʺ', '2.00 arcsec', 2000000);
insert into t_gmos_custom_slit_width values ('CustomWidth_5_00', '5.00ʺ', '5.00 arcsec', 5000000);

