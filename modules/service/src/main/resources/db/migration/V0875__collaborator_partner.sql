

-- add partner to t_program_user, defaulting existing non-support program users to 'us'

alter table t_program_user
add c_partner d_tag references t_partner (c_tag);

update t_program_user set c_partner = 'us' where c_role <> 'support';

alter table t_program_user
add check ((c_partner is null) = (c_role = 'support'));

-- add partner to t_invitation, defaulting existing non-support invitations to 'us'

alter table t_invitation
add c_partner d_tag references t_partner (c_tag);

update t_invitation set c_partner = 'us' where c_role <> 'support';

alter table t_invitation
add check ((c_partner is null) = (c_role = 'support'));

