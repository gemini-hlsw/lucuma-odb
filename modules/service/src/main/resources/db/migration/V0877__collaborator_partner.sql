

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

-- update insert_invitation to take a partner

DROP FUNCTION insert_invitation;

CREATE FUNCTION insert_invitation(
  p_issuer_id       d_user_id,                   
  p_program_id      d_program_id,         
  p_recipient_email d_email,       
  p_role            e_program_user_role,
  p_partner         d_tag,
  p_email_id        text
) RETURNS text AS $$
  DECLARE
    invitation_key_id d_invitation_id := to_hex(nextval('s_invitation_id'::regclass));
    invitation_key text := md5(random()::text) || md5(random()::text) ||  md5(random()::text);
    invitation_key_hash text := md5(invitation_key);
  BEGIN
    INSERT INTO t_invitation (
      c_invitation_id,
      c_issuer_id,      
      c_program_id,     
      c_recipient_email,
      c_role,        
      c_partner,   
      c_key_hash,
      c_email_id
    )
    VALUES (
      invitation_key_id,
      p_issuer_id,      
      p_program_id,     
      p_recipient_email,
      p_role,           
      p_partner,
      invitation_key_hash,
      p_email_id
    );
    return invitation_key_id || '.' || invitation_key;
  END;
$$ LANGUAGE plpgsql;

