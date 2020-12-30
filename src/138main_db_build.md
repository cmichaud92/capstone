---
title: "138 Database for all ISMP data"
author:
  affiliation: Utah Division of Wildlife Resources
  name: Christopher Michaud
date: "24 December 2020"
output: 
  html_document:
    keep_md: yes
---






```sql
PRAGMA foreign_keys = ON
```

## About

This is a lightweight backwards compatable batabase intended to store all seining data past and present for analysis purposes.  This may be transient as ISMP data will be integrated into Moab_db and could be queried from that location.  However, it may be a while before this happens.

This database creation program will not overwrite an existing database, however, best not poke the bear...

## Meta table


```sql
-- Consider incorporating something like this
-- Its just difficult determining 
CREATE TABLE meta (
id_meta INTEGER NOT NULL UNIQUE PRIMARY KEY,
cd_study TEXT NOT NULL,
`year` INTEGER NOT NULL UNIQUE,
principal_fname TEXT NOT NULL,                      
principal_lname TEXT NOT NULL,
cd_agency TEXT NOT NULL CHECK (agency IN ('UDWR-M', 'UDWR-V', 'CSU', 'FWS-GJ', 'FWS-V')),
datatype TEXT NOT NULL CHECK (data_type IN ('EL', 'SE', 'TR', 'ANT', 'OT'))
);
```




```sql
CREATE UNIQUE INDEX idx_meta_site ON meta (project_code, `year`);
```

## Site Table


```sql
CREATE TABLE site (
id_site TEXT NOT NULL UNIQUE PRIMARY KEY,
cd_study TEXT NOT NULL,
year INTEGER NOT NULL,
--dt_site TEXT NOT NULL,
cd_rvr TEXT NOT NULL CHECK (cd_rvr IN ('CO', 'GR', 'OT')),
cd_rch TEXT NOT NULL CHECK (cd_rch IN ('LGR', 'LCO', 'MGR', 'OT')),
rmi_bel REAL NOT NULL CHECK (rmi_bel >= 0 & rmi_bel <= 320),  
hab_class INTEGER NOT NULL CHECK (hab_class >= 1 & hab_class <= 4),
mc_secchi INTEGER,
mc_temp REAL,
hab1 TEXT NOT NULL CHECK (hab1 IN ('BA', 'ED', 'EM', 'FB', 'FT', 'ID', 'IT', 'IP', 'LK', 'MC', 'OI', 'PO', 'RI', 'RU', 'SH', 'SC', 'OT')),
hab2 TEXT NOT NULL CHECK (hab2 IN ('BA', 'ED', 'EM', 'FB', 'FT', 'ID', 'IT', 'IP', 'LK', 'MC', 'OI', 'PO', 'RI', 'RU', 'SH', 'SC', 'OT')),
hab_geom TEXT CHECK (hab_geom IN ('SC', 'MS', 'HS', 'SE', 'LV', 'IP', 'FT')),
hab_length INTEGER,
hab_width INTEGER,
hab_max_depth INTEGER,
hab_secchi INTEGER,
hab_temp REAL,
hab_aspect INTEGER,
flow INTEGER CHECK (flow BETWEEN 0 AND 1),
cover_typ TEXT,
cover_pct INTEGER,
crew TEXT,
loc_x REAL,
loc_y REAL,
epsg INTEGER,
site_notes TEXT,
upload_id INTEGER
);
```

## Haul Table


```sql
CREATE TABLE haul (
id_haul TEXT NOT NULL UNIQUE PRIMARY KEY,
id_site TEXT NOT NULL,
tm_start TEXT,
haul_num INTEGER, 
haul_length INTEGER,
haul_width INTEGER,
method TEXT NOT NULL CHECK (method IN ('A', 'P', 'UNK')), --check!!
d1_depth INTEGER,
d1_sub1 TEXT CHECK (d1_sub1 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', NULL)),
d1_sub2 TEXT CHECK (d1_sub2 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', NULL)), 
d2_depth INTEGER,
d2_sub1 TEXT CHECK (d2_sub1 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', NULL)), 
d2_sub2 TEXT CHECK (d2_sub2 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', NULL)), 
dmax_depth INTEGER,
dmax_sub1 TEXT CHECK (dmax_sub1 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', NULL)), 
dmax_sub2 TEXT CHECK (dmax_sub2 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', NULL)), 
haul_notes TEXT,
FOREIGN KEY (id_site) REFERENCES site(id_site)
);
```

## Fish Table


```sql
CREATE TABLE fish (
id_fish INTEGER PRIMARY KEY,
id_haul TEXT NOT NULL,
cd_spp TEXT NOT NULL CHECK (cd_spp IN ('BCT', 'BH', 'BHRZ', 'BT', 'CH', 'CS', 'FB', 'FM', 'FR', 'HB', 'MF', 'RT', 'RZ', 'SD', 'SU', 'BB', 'BC', 'BG', 'BN', 'BS', 'CC', 'CP', 'FH', 'GA', 'GC', 'GS', 'GZ', 'KO', 'LG', 'NP', 'PK', 'RB', 'RD', 'RS', 'SB', 'SM', 'SS', 'UC', 'WB', 'WC', 'WE', 'WF', 'WS', 'YB', 'YP', 'OT', 'UM', 'UI', 'UNK')),
tot_length INTEGER CHECK (tot_length != 0), 
weight INTEGER CHECK (weight != 0),
disp TEXT CHECK (disp IN ('CT', 'DE', 'RT', 'FC', 'HA', 'DF', 'SS', 'DP', 'RA', 'TR', 'TL', 'UNK', 'OT', NULL)),
pit_num TEXT,
pit_recap INTEGER CHECK(pit_recap BETWEEN 0 AND 1),
fish_notes TEXT,
FOREIGN KEY (id_haul) REFERENCES haul(id_haul)
);
```

## Count Table


```sql
CREATE TABLE inclusive_count (
id_count INTEGER PRIMARY KEY,
id_haul TEXT NOT NULL,
cd_spp TEXT NOT NULL CHECK (cd_spp IN ('BCT', 'BH', 'BHRZ', 'BT', 'CH', 'CS', 'FB', 'FM', 'FR', 'HB', 'MF', 'RT', 'RZ', 'SD', 'SU', 'BB', 'BC', 'BG', 'BN', 'BS', 'CC', 'CP', 'FH', 'GA', 'GC', 'GS', 'GZ', 'KO', 'LG', 'NP', 'PK', 'RB', 'RD', 'RS', 'SB', 'SM', 'SS', 'UC', 'WB', 'WC', 'WE', 'WF', 'WS', 'YB', 'YP', 'OT', 'UM', 'UI', 'UNK')),
n_fish INTEGER NOT NULL,
count_notes TEXT,
FOREIGN KEY (id_haul) REFERENCES haul(id_haul)
);

```

## Transect table


```sql
CREATE TABLE trans (
id_trans INTEGER PRIMARY KEY,
id_site TEXT,
location INTEGER,
width INTEGER,
secchi INTEGER,
temp REAL,
--flow TEXT CHECK (flow IN ('Y', 'N')),
d1_depth INTEGER,
d1_totdep INTEGER,
d1_sub1 TEXT CHECK (d1_sub1 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', 'UNK')),
d1_sub2 TEXT CHECK (d1_sub2 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', 'UNK')),
d2_depth INTEGER,
d2_totdep INTEGER,
d2_sub1 TEXT CHECK (d2_sub1 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', 'UNK')),
d2_sub2 TEXT CHECK (d2_sub2 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', 'UNK')),
d3_depth INTEGER,
d3_totdep INTEGER,
d3_sub1 TEXT CHECK (d3_sub1 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', 'UNK')),
d3_sub2 TEXT CHECK (d3_sub2 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', 'UNK')),
dmax_depth INTEGER,
dmax_totdep INTEGER,
dmax_sub1 TEXT CHECK (dmax_sub1 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', 'UNK')),
dmax_sub2 TEXT CHECK (dmax_sub2 IN ('SI', 'SA', 'GR', 'RU', 'BO', 'OT', 'UNK')),
trans_notes TEXT,
FOREIGN KEY (id_site) REFERENCES site(id_site)
);

```


```r
dbDisconnect(con)
```

