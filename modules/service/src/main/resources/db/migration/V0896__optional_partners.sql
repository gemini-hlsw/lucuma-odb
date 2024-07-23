-- We now allow users to be not associated with a partner
alter table t_program_user
drop constraint t_program_user_check;

alter table t_invitation
drop constraint t_invitation_check;
