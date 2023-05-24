-- Store the S3 file path in the attachments tables instead of 
-- just the UUID used to build the path. This allows us to change the
-- file key used in the future without "losing" attachments.
alter table t_obs_attachment
add column c_remote_path text;

update t_obs_attachment
set c_remote_path = c_program_id || '/obs/' || c_remote_id;

alter table t_obs_attachment
  alter column c_remote_path set not null,
  add check (length(c_remote_path) > 0),
  drop column c_remote_id;

alter table t_proposal_attachment
add column c_remote_path text;

update t_proposal_attachment
set c_remote_path = c_program_id || '/proposal/' || c_remote_id;

alter table t_proposal_attachment
  alter column c_remote_path set not null,
  add check (length(c_remote_path) > 0),
  drop column c_remote_id;
