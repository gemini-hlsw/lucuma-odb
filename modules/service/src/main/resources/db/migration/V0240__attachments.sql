-- ATTACHMENT TYPES

create table t_attachment_type (
  c_tag          d_tag     not null primary key,
  c_short_name   varchar   not null,
  c_long_name    varchar   not null
);

insert into t_attachment_type values ('proposal', 'Proposal', 'Proposal');
insert into t_attachment_type values ('finder', 'Finder', 'Finder Chart');
insert into t_attachment_type values ('mos_mask', 'MOS Mask', 'MOS Mask');
insert into t_attachment_type values ('pre_imaging', 'Pre-Imaging', 'Pre-Imaging');

-- ATTACHMENT FILES

create domain d_attachment_id as varchar; -- TODO format check
comment on domain d_attachment_id is 'GID for program file attachments';

create sequence s_attachment_id START with 256; -- three hex digits
create table t_attachment (
  c_program_id        d_program_id      not null    references t_program(c_program_id),
  c_attachment_id     d_attachment_id   primary key default 'a-' || to_hex(nextval('s_attachment_id')),
  c_attachment_type   d_tag             not null    references t_attachment_type(c_tag),
  c_file_name         text              not null    check(length(c_file_name) > 0),
  c_description       text              check (c_description is null or length(c_description) > 0),
  c_checked           boolean           not null    default false,
  c_file_size         bigint            not null,
  c_updated_at        timestamp         not null    default CURRENT_TIMESTAMP,

  unique (c_program_Id, c_attachment_id),
  unique (c_program_Id, c_file_name)
);

-- UDPATE c_updated_at on UPDATE

CREATE OR REPLACE FUNCTION ch_update_updated_at()
  RETURNS trigger AS $$
DECLARE
BEGIN
  NEW.c_updated_at = now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_update_updated_at_trigger
  BEFORE UPDATE ON t_attachment
  FOR EACH ROW
  EXECUTE PROCEDURE ch_update_updated_at();
