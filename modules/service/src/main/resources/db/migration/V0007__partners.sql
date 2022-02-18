create table t_partner (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null,
  c_active            boolean     not null default true,
  c_sites             e_site[]    not null
);

insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('ar', 'Argentina', 'Argentina', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('br', 'Brazil', 'Brazil', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('ca', 'Canada', 'Canada', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('cfh', 'CFHT', 'Canada-France-Hawaii Telescope', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('cl', 'Chile', 'Chile', array['gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('keck', 'Keck', 'Keck Observatory', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('kr', 'Korea', 'Republic of Korea', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('lp', 'Long Programs', 'Long Programs', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('subaru', 'Subaru', 'Subaru Telescope', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('uh', 'University of Hawaii', 'University of Hawaii', array['gn']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('us', 'United States', 'United States', array['gn', 'gs']::e_site[]);
insert into t_partner (c_tag, c_short_name, c_long_name, c_sites) values ('gt', 'Guaranteed Time', 'Guaranteed Time', array['gn', 'gs']::e_site[]);
