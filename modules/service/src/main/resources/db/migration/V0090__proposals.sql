
-- TAC groups

create table t_tac_group (
  c_tag   d_tag       not null primary key,
  c_name  varchar     not null
);

insert into t_tac_group values ('solar_system', 'Solar System');
insert into t_tac_group values ('exoplanets', 'Exoplanets');
insert into t_tac_group values ('galactic_local_group', 'Galactic/Local Group');
insert into t_tac_group values ('extragalactic', 'Extragalactic');

-- TAC Categories

create table t_tac_category (
  c_tag   d_tag       not null primary key,
  c_name  varchar     not null,
  c_category d_tag    not null references t_tac_group(c_tag)
);

insert into t_tac_category values('small_bodies', 'Small Bodies: Asteroids, Comets, Moons, Kuiper Belt', 'solar_system');
insert into t_tac_category values('planetary_atmospheres', 'Planetary Atmospheres', 'solar_system');
insert into t_tac_category values('planetary_surfaces', 'Planetary Surfaces', 'solar_system');
insert into t_tac_category values('solar_system_other', 'Solar System Other', 'solar_system');
insert into t_tac_category values('exoplanet_radial_velocities', 'Exoplanet Radial Velocities', 'exoplanets');
insert into t_tac_category values('exoplanet_atmospheres_activity', 'Exoplanet Atmospheres/Activity', 'exoplanets');
insert into t_tac_category values('exoplanet_transits', 'Exoplanet Transits, Rossiter McLaughlin', 'exoplanets');
insert into t_tac_category values('exoplanet_host_star', 'Exoplanet Host Star Properties/Connections', 'exoplanets');
insert into t_tac_category values('exoplanet_other', 'Exoplanet Other', 'exoplanets');
insert into t_tac_category values('stellar_astrophysics', 'Stellar Astrophysics, Evolution, Supernovae, Abundances', 'galactic_local_group');
insert into t_tac_category values('stellar_populations', 'Stellar Populations, Clusters, Chemical Evolution', 'galactic_local_group');
insert into t_tac_category values('star_formation', 'Star Formation', 'galactic_local_group');
insert into t_tac_category values('gaseous_astrophysics', 'Gaseous Astrophysics, H II regions, PN, ISM, SN remnants, Novae', 'galactic_local_group');
insert into t_tac_category values('stellar_remnants', 'Stellar Remnants/Compact Objects, WD, NS, BH', 'galactic_local_group');
insert into t_tac_category values('galactic_other', 'Galactic Other', 'galactic_local_group');
insert into t_tac_category values('cosmology', 'Cosmology, Fundamental Physics, Large Scale Structure', 'extragalactic');
insert into t_tac_category values('clusters_of_galaxies', 'Clusters/Groups of Galaxies', 'extragalactic');
insert into t_tac_category values('high_z_universe', 'High-z Universe', 'extragalactic');
insert into t_tac_category values('low_z_universe', 'Low-z Universe', 'extragalactic');
insert into t_tac_category values('active_galaxies', 'Active Galaxies, Quasars, SMBH', 'extragalactic');
insert into t_tac_category values('extragalactic_other', 'Extragalactic Other', 'extragalactic');

-- Proposal Classes

create table t_proposal_class (
  c_tag   d_tag       not null primary key,
  c_name  varchar     not null
);

insert into t_proposal_class values ('large_program', 'Large Program Observing at Gemini');
insert into t_proposal_class values ('fast_turnaround', 'Fast Turnaround Observing at Gemini');
insert into t_proposal_class values ('queue', 'Queue Observing at Gemini');
insert into t_proposal_class values ('classical', 'Classical Observing at Gemini');
insert into t_proposal_class values ('exchange', 'Exchange Observing at Keck/Subaru');
insert into t_proposal_class values ('intensive', 'Intensive Program Observing at Subaru');
insert into t_proposal_class values ('demo_science', 'Demo Science');
insert into t_proposal_class values ('directors_time', 'Directors Time');
insert into t_proposal_class values ('poor_weather', 'Poor Weather');
insert into t_proposal_class values ('system_verification', 'System Verification');

-- Proposals

create table t_proposal (
  c_program_id        d_program_id      not null  primary key references t_program(c_program_id) on delete cascade,
  c_title             text              null      check (c_title is null or length(c_title) > 0),
  c_abstract          text              null      check (c_abstract is null or length(c_abstract) > 0),
  c_category          d_tag             null      references t_tac_category(c_tag),
  c_too_activation    e_too_activation  null,
  c_class             d_tag             not null  references t_proposal_class(c_tag),
  c_min_percent       d_int_percentage  not null,
  c_min_percent_total d_int_percentage  null      check((c_min_percent_total is not null) = (c_category = 'intensive' or c_category = 'large_program')),
  c_totalTime         interval          null      check((c_totalTime is not null) = (c_category = 'intensive' or c_category = 'large_program'))
);

-- Partner Splits

create table t_partner_split (
  c_program_id        d_program_id      not null  references t_program(c_program_id) on delete cascade,
  c_partner           d_tag             not null  references t_partner(c_tag),
  c_percent           d_int_percentage  not null,
  unique(c_program_id, c_partner)
);

