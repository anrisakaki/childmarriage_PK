library(kableExtra)
library(tidyverse)
library(haven)
library(sf)
library(fixest)
library(extrafont)
library(lubridate)
library(sjPlot)
library(sfheaders)
library(mgrs)
library(broom)
library(patchwork)
library(srvyr)
library(survey)

# Pakistan SHP
pkmap_1 <- read_sf("SHP/gadm41_PAK_shp/gadm41_PAK_1.shp")
pkmap_2 <- read_sf("SHP/gadm41_PAK_shp/gadm41_PAK_2.shp")
pkmap_3 <- read_sf("SHP/gadm41_PAK_shp/gadm41_PAK_3.shp")

dist_shp <- read_sf("SHP/pak_adm_wfp_20220909_shp/pak_admbnda_adm2_wfp_20220909.shp")

dist_pk <- read_sf("SHP/UC-shapefile/Adminbdy Shapefile/District_Boundary.shp")
uc_pk <- read_sf("SHP/UC-shapefile/Adminbdy Shapefile/Union_Council.shp")

flood10_shp <- read_sf("SHP/pak_adm4_2010floodaffected_pco_20150826/Pak_adm4_2010FloodAffected_pco_20150826/Pak_adm4_2010FloodAffected_pco_20150826.shp")

dhs_shp <- read_sf("data/geo_pk1991_2017/geo_pk1991_2017.shp")
dhs_dist_06_shp <- read_sf("data/PKGE51FL/PKGE51FL.shp")
dhs_dist_17_shp <- read_sf("data/PKGE71FL/PKGE71FL.shp")

# SUCCESS DATA
succ <- read_sav("data/data/EL-HH Rooster Data Combined V3.sav")

# MICS
balochistan_hl <- read_sav("data/MICS/Pakistan (Balochistan) MICS6 Datasets/Pakistan (Baluchistan) SPSS Datasets/hl.sav")
balochistan_wm <- read_sav("data/MICS/Pakistan (Balochistan) MICS6 Datasets/Pakistan (Baluchistan) SPSS Datasets/wm.sav")

kp_hl <- read_sav("data/MICS/Pakistan Khyber Pakhtunkhwa MICS6 Datasets/Pakistan Khyber Pakhtunkhwa MICS6 SPSS Datasets/hl.sav")
kp_wm <- read_sav("data/MICS/Pakistan Khyber Pakhtunkhwa MICS6 Datasets/Pakistan Khyber Pakhtunkhwa MICS6 SPSS Datasets/wm.sav")

punjab_hl <- read_sav("data/MICS/Pakistan Punjab MICS6 Datasets/Pakistan Punjab MICS6 Datasets/hl.sav")
punjab_wm <- read_sav("data/MICS/Pakistan Punjab MICS6 Datasets/Pakistan Punjab MICS6 Datasets/wm.sav")

sindh_hl <- read_sav("data/MICS/Pakistan Sindh MICS6 Datasets/Pakistan Sindh MICS6 SPSS Datasets/hl.sav")
sindh_wm <- read_sav("data/MICS/Pakistan Sindh MICS6 Datasets/Pakistan Sindh MICS6 SPSS Datasets/wm.sav")

# DHS 
dhs_hh_06 <- read_sav("data/DHS/PDHS 2006-07/PKHR53SV/PKHR53FL.SAV")
dhs_w_06 <- read_sav("data/DHS/PDHS 2006-07/PKIR53SV/PKIR53FL.SAV")
dhs_hh_12 <- read_sav("data/DHS/PDHS 2012-13/PKHR61SV/PKHR61FL.SAV")
dhs_w_12 <- read_sav("data/DHS/PDHS 2012-13/PKIR61SV/PKIR61FL.SAV")
dhs_hh_17 <- read_sav("data/DHS/PDHS 2017-18/PKHR71SV/PKHR71FL.SAV")
dhs_w_17 <- read_sav("data/DHS/PDHS 2017-18/PKIR71SV/PKIR71FL.SAV")
