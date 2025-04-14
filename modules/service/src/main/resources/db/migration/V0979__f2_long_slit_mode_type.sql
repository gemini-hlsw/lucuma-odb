alter type e_observing_mode_type add value 'flamingos_2_long_slit';

alter table t_f2_readout_mode RENAME TO t_f2_read_mode;
alter table f2_readout_mode RENAME TO t_f2_readout_mode;

truncate table t_f2_read_mode;
alter table t_f2_read_mode
  drop column c_read_count;
alter table t_f2_read_mode
  add column c_reads d_tag not null references t_f2_reads(c_tag);

insert into t_f2_read_mode values ('Bright', 'bright', 'Bright Object', 'Strong Source', '1500 millisecond', '5000 millisecond', '8000 millisecond', 11.7, 'reads_1');
insert into t_f2_read_mode values ('Medium', 'medium', 'Medium Object', 'Medium Source', '6 second',         '21 second',        '14 second',        6.0, 'reads_4');
insert into t_f2_read_mode values ('Faint',  'faint',  'Faint Object',  'Weak Source',   '12 second',        '85 second',        '20 second',        5.0, 'reads_8');

