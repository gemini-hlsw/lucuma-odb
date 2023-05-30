-- Allowed file extensions for obs attachments

create table t_obs_attachment_file_ext (
  c_attachment_type   d_tag   not null  references t_obs_attachment_type(c_tag),
  c_file_extension    text    not null  check(c_file_extension ~ '^[\da-z]+$'),

  constraint t_obs_attachment_file_ext_pkey primary key (c_attachment_type, c_file_extension)
);

insert into t_obs_attachment_file_ext values ('finder', 'jpg');
insert into t_obs_attachment_file_ext values ('finder', 'png');
insert into t_obs_attachment_file_ext values ('mos_mask', 'fits');
insert into t_obs_attachment_file_ext values ('pre_imaging', 'fits');
