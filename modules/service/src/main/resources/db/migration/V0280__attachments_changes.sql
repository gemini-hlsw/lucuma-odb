-- make current attachments observation specific
alter table t_attachment
rename to t_obs_attachment;

alter table t_obs_attachment
rename c_attachment_id to c_obs_attachment_id;

alter table t_obs_attachment
add column c_remote_id uuid not null;

delete from t_attachment_type
where c_tag = 'proposal';

alter table t_attachment_type
rename to t_obs_attachment_type;

alter sequence s_attachment_id
rename to s_obs_attachment_id;

alter domain d_attachment_id
rename to d_obs_attachment_id;

-- obs attachment id format constraint
alter table t_obs_attachment
add constraint id_format CHECK(c_obs_attachment_id ~ '^a-[\da-f]+$');
