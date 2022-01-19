create table t_partner (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null,
  c_active            boolean     not null default true,
  c_sites             e_site[]    not null
);

insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('AR', 'Argentina', 'Argentina', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('BR', 'Brazil', 'Brazil', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('CA', 'Canada', 'Canada', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('CFH', 'CFHT', 'Canada-France-Hawaii Telescope', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('CL', 'Chile', 'Chile', array['gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('KECK', 'Keck', 'Keck Observatory', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('KR', 'Korea', 'Republic of Korea', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('LP', 'Long Programs', 'Long Programs', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('SUBARU', 'Subaru', 'Subaru Telescope', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('UH', 'University of Hawaii', 'University of Hawaii', array['gn']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('US', 'United States', 'United States', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('GT', 'Guaranteed Time', 'Guaranteed Time', array['gn', 'gs']::e_site[]);
