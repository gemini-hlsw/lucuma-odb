
create domain d_invitation_id as varchar;
create sequence s_invitation_id start with 256;

create type e_invitation_status as enum('pending', 'redeemed', 'declined', 'revoked');

create table t_invitation (
  c_invitation_id   d_invitation_id              not null primary key,
  c_status          e_invitation_status          not null default 'pending',
  c_issuer_id       d_user_id                    not null references t_user (c_user_id),
  c_program_id      d_program_id                 not null references t_program (c_program_id),
  c_role            e_program_user_role          not null,
  c_support_type    e_program_user_support_type  default null,
  c_support_partner d_tag                        default null references t_partner(c_tag), -- check constraint below
  c_key_hash        char(32)                     not null,
  c_redeemer_id     d_user_id                    null references t_user (c_user_id), -- id of user who redeemed, if any
  check (
    (c_support_type = 'partner' and c_support_partner is not null) or
    (c_support_partner is null)
  )
  -- todo: check that redeemer and status are consistent
);

CREATE OR REPLACE FUNCTION insert_invitation(
  p_issuer_id       d_user_id,                   
  p_program_id      d_program_id,                
  p_role            e_program_user_role,         
  p_support_type    e_program_user_support_type, 
  p_support_partner d_tag                  
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
      c_role,           
      c_support_type,   
      c_support_partner,
      c_key_hash
    )
    VALUES (
      invitation_key_id,
      p_issuer_id,      
      p_program_id,     
      p_role,           
      p_support_type,   
      p_support_partner,
      invitation_key_hash
    );
    return invitation_key_id || '.' || invitation_key;
  END;
$$ LANGUAGE plpgsql;
