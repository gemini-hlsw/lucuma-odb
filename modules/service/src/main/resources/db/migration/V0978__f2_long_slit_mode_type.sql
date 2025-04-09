alter type e_observing_mode_type add value 'flamingos_2_long_slit';

alter table t_f2_readout_mode RENAME TO t_f2_read_mode;
alter table f2_readout_mode RENAME TO t_f2_readout_mode;
