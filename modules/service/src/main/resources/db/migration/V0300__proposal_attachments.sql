-- PROPOSAL ATTACHMENT TYPES

create table t_proposal_attachment_type (
  c_tag          d_tag     not null primary key,
  c_short_name   varchar   not null,
  c_long_name    varchar   not null
);

insert into t_proposal_attachment_type values ('science', 'Science Case', 'Science Case & Design');
insert into t_proposal_attachment_type values ('team', 'Team Info', 'Team Info, Previous Use, etc.');

-- PROPOSAL ATTACHMENT FILES

create table t_proposal_attachment (
  c_program_id        d_program_id      not null    references t_program(c_program_id),
  c_attachment_type   d_tag             not null    references t_proposal_attachment_type(c_tag),
  c_file_name         text              not null    check(length(c_file_name) > 0),
  c_description       text              check (c_description is null or length(c_description) > 0),
  c_checked           boolean           not null    default false,
  c_file_size         bigint            not null,
  c_updated_at        timestamp         not null    default CURRENT_TIMESTAMP,
  c_remote_id         uuid              not null,

  constraint t_proposal_attachment_pkey primary key (c_program_id, c_attachment_type),
  unique (c_program_id, c_file_name)
);

CREATE TRIGGER ch_proposal_attachment_update_updated_at_trigger
  BEFORE UPDATE on t_proposal_attachment
  FOR EACH ROW
  EXECUTE PROCEDURE ch_update_updated_at();
