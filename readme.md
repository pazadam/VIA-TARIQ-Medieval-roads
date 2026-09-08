# From cursus publicus to barīd: change and persistency of the terrestrial communication routes in the Levant/Bilād al-Šām

This repository contains input road and site data and R code used for network analysis and comparison of the Roman and Medieval road network in the Levant/Bilād al-Šām. The repository serves as a supplementary data to the article 'From cursus publicus to barīd: change and persistency of the terrestrial communication routes in the Levant/Bilād al-Šām'.

### Introduction

This research focuses on the persistency of the Roman road network into the Medieval period (Early-Middle Islamic, ca. 650-1517 CE) in the Levant/Bilād al-Šām (former Roman provinces of Syria, Judaea, and Arabia). This R code employs network and spatial analytical methods to explore a) change and persistency of important nodal points using degree and betweenness centrality, b) change and persistency of important roads using edge betweenness centrality, and c) analyse clustering of nodal points across the periods.

The Roman road data are based on the [Itiner-e](https://itiner-e.org) dataset. The Medieval road data for the Levant were collected in course of the EU Horizon MSCA project 'VIA-TARIQ: Analysing the long-term change and persistency of the Roman road system in the Levant'. The Medieval road data, including bibliography used in the data collection, are available at the [zenodo](https://doi.org/10.5281/zenodo.21981430) repository.

### Data

[/data](https://github.com/pazadam/VIA-TARIQ-Medieval-roads/tree/main/data) folder contains input road and site data used in the analysis.

Structure of the *roads_early_islamic/middle_islamic/early_mamluk/late_mamluk* files:

| field name | type | description |
|----|----|----|
| id | integer | unique numerical identifier |
| name | string | name of the road segment |
| type | string | 'Main Road' or 'Secondary Road' |
| typeCode | integer | numeric code of the type, 'Main' = 2, 'Secondary' = 1 |
| roadCer | string | certainty of the existence of the road segment based on the available sources, 'High', 'Medium', 'Low' |
| lengthGeo | double | geodesic length in meteres, using EPSG:3395 crs |
| avgSlope | double | average slope in degrees |
| pace | double | inverse of walking speed (s/m) calculated using Tobler's hiking function and average slope of the road segment |
| timeWeight | double | time weight calculated as a time needed to cross the road segment (geodesic length x pace) |
| hajj | string | whether road segment belongs to a Hajj route |
| barid | string | whether a road segment belongs to a barid route (only Early Islamic, Early and Late Mamluk datasets) |

*roads_roman* file has slightly different data structure:

| field name | type | description |
|----|----|----|
| id | integer | unique numerical identifier |
| type | string | 'Main Road' or 'Secondary Road' |
| typeWeight | integer | numeric code of the type, 'Main' = 2, 'Secondary' = 1 |
| lengthGeo | double | geodesic length in meteres, using EPSG:3395 crs |
| avgSlope | double | average slope in degrees |
| pace | double | inverse of walking speed (s/m) calculated using Tobler's hiking function and average slope of the road segment |
| timeWeight | double | time weight calculated as a time needed to cross the road segment (geodesic length x pace) |
| itAnt | string | whether road segment belongs to Itinerarium Antonini route |

Structure of the *sites_early_islamic/middle_islamic/early_mamluk/late_mamluk* files:

| field name | type | description |
|----|----|----|
| id | integer | unique numerical identifier |
| name | string | Medieval name of the site |
| modernName | string | modern name of the site |
| type | string | 'settlement', 'city', 'station', 'fort', 'place', 'sanctuary', 'bridge' |
| minDate | integer | earliest date the site can be dated to, for unknown 9999 is used |
| maxDate | integer | date of destruction/abandonment, for sites in continual use or unknown 9999 is used |
| umayyad | string | whether site exists in given period 'yes', 'no', 'possible', 'unknown' |
| abbasid | string | whether site exists in given period 'yes', 'no', 'possible', 'unknown' |
| fatimid | string | whether site exists in given period 'yes', 'no', 'possible', 'unknown' |
| ayyubidCru | string | whether site exists in given period 'yes', 'no', 'possible', 'unknown' |
| mamluk | string | whether site exists in given period 'yes', 'no', 'possible', 'unknown' |
| citation | string | author of the digitisation |
| biblio | string | bibliographical reference (Name Year) used in the digitisation (full bibliography at zenodo) |
| conn | integer | connectivity value, calculated from the 'typeWeight' values of road segments that touch the site |

*sites_roman* file has different data structure:

| field name | type | description |
|------------------------|------------------------|------------------------|
| id | integer | unique numerical identifier |
| name | string | Medieval name of the site |
| geoContext | string | modern name of the site |
| featureTyp | string | 'settlement', 'city', 'station', 'fort', 'place', 'sanctuary', 'bridge' |
| maxDate | integer | date of destruction/abandonment, for sites in continual use or unknown 9999 is used |
| minDate | integer | earliest date the site can be dated to, for unknown 9999 is used |
| source | string | bibliographical reference (Name Year) used in the digitisation |
| location | string | whether the location of the representative point was moved to a road segment, 'precise' = not moved, 'altered' = moved |
| hel | string | whether site exists in given period 'yes', 'no' |
| rom | string | whether site exists in given period 'yes', 'no' |
| byz | string | whether site exists in given period 'yes', 'no' |
| conn | integer | connectivity value, calculated from the 'typeWeight' values of road segments that touch the site |

### Dependencies

The code depends on the following R packages:

- [sf](https://cran.r-project.org/web/packages/sf/index.html)

- [sfnetworks](https://cran.r-project.org/web/packages/sfnetworks/refman/sfnetworks.html)

- [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)

- [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)

- [igraph](https://cran.r-project.org/web/packages/igraph/index.html)

- [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)

### Funding information

This research was undertaken in the project '*VIA-TARIQ: Analysing the long-term change and persistency of the Roman road system in the Levant*'. This project has received funding from the European Union's Horizon Europe research and innovation programme under the Marie Sklodowska-Curie grant agreement No 101151931.
