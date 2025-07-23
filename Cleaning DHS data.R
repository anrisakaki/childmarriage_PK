# 2006 
names(dhs_hh_06) <- make.names(names(dhs_hh_06), unique = TRUE)
names(dhs_hh_06) <- gsub("\\$", "_", names(dhs_hh_06))  
names(dhs_hh_06) <- gsub("[^a-zA-Z0-9_]", "_", names(dhs_hh_06))  
names(dhs_hh_06) <- tolower(names(dhs_hh_06))

names(dhs_w_06) <- make.names(names(dhs_w_06), unique = TRUE)
names(dhs_w_06) <- gsub("\\$", "_", names(dhs_w_06))  
names(dhs_w_06) <- gsub("[^a-zA-Z0-9_]", "_", names(dhs_w_06))  
names(dhs_w_06) <- tolower(names(dhs_w_06))

write_dta(dhs_hh_06, "dhs_hh_06.dta")
write_dta(dhs_w_06, "dhs_w_06.dta")

# 2017
names(dhs_w_12) <- make.names(names(dhs_w_12), unique = TRUE)
names(dhs_w_12) <- gsub("\\$", "_", names(dhs_w_12))  
names(dhs_w_12) <- gsub("[^a-zA-Z0-9_]", "_", names(dhs_w_12))  
names(dhs_w_12) <- tolower(names(dhs_w_12))

names(dhs_hh_12) <- make.names(names(dhs_hh_12), unique = TRUE)
names(dhs_hh_12) <- gsub("\\$", "_", names(dhs_hh_12))  
names(dhs_hh_12) <- gsub("[^a-zA-Z0-9_]", "_", names(dhs_hh_12))  
names(dhs_hh_12) <- tolower(names(dhs_hh_12))

write_dta(dhs_hh_12, "dhs_hh_12.dta")
write_dta(dhs_w_12, "dhs_w_12.dta")

# 2017
names(dhs_w_17) <- make.names(names(dhs_w_17), unique = TRUE)
names(dhs_w_17) <- gsub("\\$", "_", names(dhs_w_17))  
names(dhs_w_17) <- gsub("[^a-zA-Z0-9_]", "_", names(dhs_w_17))  
names(dhs_w_17) <- tolower(names(dhs_w_17))

names(dhs_hh_17) <- make.names(names(dhs_hh_17), unique = TRUE)
names(dhs_hh_17) <- gsub("\\$", "_", names(dhs_hh_17))  
names(dhs_hh_17) <- gsub("[^a-zA-Z0-9_]", "_", names(dhs_hh_17))  
names(dhs_hh_17) <- tolower(names(dhs_hh_17))

write_dta(dhs_hh_17, "dhs_hh_17.dta")
write_dta(dhs_w_17, "dhs_w_17.dta")

clean_dhs_hh_fn <- function(i){
  i %>% 
    rename(
      cluster = hv001,
      hh = hv002,
      hhwt = hv005,
      regionid = hv024,
      districtid = shdist,
      hhsize = hv013,
      agri_land = hv244,
      livestock = hv246
    ) %>% 
    select(cluster, hh, regionid, districtid, agri_land, livestock, hhsize, hhwt)
}

clean_dhs_w_fn <- function(i){
  i %>% 
    rename(districtid = sdist,
           cluster = v001,
           hh = v002,
           iv = v003,
           monthint = v006,
           yrint = v007,
           birthda = v011,
           birthmo = v009,
           birthyr = v010,
           regionid = v024,
           urban = v025,
           age = v012,
           educattain = v106,
           yrschool = v133,
           head_age = v152,
           wealthind = v190,
           chborn = v201,
           agemarr = v511,
           agefbirth = v212,
           nchild = v218,
           marr_fbirth = v221,
           marst = v501,
           husband_educ = v701,
           husband_ind = v704,
           husband_indgen = v705,
           husband_age = v730,
           work = v714,
           ind = v716,
           indgen = v717,
           dm1 = v743a,
           dm2 = v743b,
           dm3 = v743d,
           dm4 = v743f,
           dv1 = v744a,
           dv2 = v744b,
           dv3 = v744c,
           dv4 = v744d,
           dv5 = v744e,
           ipv1 = d105a,
           ipv2 = d105b,
           ipv3 = d105c,
           ipv4 = d105d,
           ipv5 = d105e,
           ipv6 = d105f,
           ipv7 = d105h,
           ipv8 = d105i,
           ipv9 = d105j,
           ipv10 = d105k,
           ipv11 = d110a,
           ipv12 = d110b,
           ipv13 = d110d,
           less_viol = d106,
           severe_viol = d107,
           sexual_viol = d108,
           wt = v005,
           dv_wt = d005) %>% 
    mutate(
      urban = ifelse(urban == 1, 1, 0),
      dm1_recode = ifelse(dm1 < 3, 1, 0),
      dm2_recode = ifelse(dm2 < 3, 1, 0),
      dm3_recode = ifelse(dm3 < 3, 1, 0),
      dm4_recode = ifelse(dm4 < 3, 1, 0),
      # The higher the dm_index, the more decision making power the woman has 
      dm_index = dm1_recode + dm2_recode + dm3_recode + dm4_recode,
      dv1_recode = ifelse(dv1 == 1, 1, 0),
      dv2_recode = ifelse(dv2 == 1, 1, 0),
      dv3_recode = ifelse(dv3 == 1, 1, 0),
      dv4_recode = ifelse(dv4 == 1, 1, 0),
      dv5_recode = ifelse(dv5 == 1, 1, 0),
      # The higher the dv_index, the more tolerant they are of IPV
      dv_index = dv1_recode + dv2_recode + dv3_recode + dv4_recode + dv5_recode,
      any_viol = if_else(if_any(ends_with("viol"), ~ . == 1), 1, 0),
      u16 = ifelse(agemarr < 16, 1, 0),
      u18 = ifelse(agemarr < 18, 1, 0),
      husband_agri = ifelse(husband_indgen == 4, 1, 0)
    )
}

# 2006 

dhs06_hh <- dhs_hh_06 %>% 
  clean_dhs_hh_fn()

dhs06 <- dhs_w_06 %>% 
  rename(districtid = sdist,
         cluster = v001,
         hh = v002,
         iv = v003,
         monthint = v006,
         yrint = v007,
         birthda = v011,
         birthmo = v009,
         birthyr = v010,
         regionid = v024,
         urban = v025,
         age = v012,
         educattain = v106,
         yrschool = v133,
         head_age = v152,
         wealthind = v190,
         chborn = v201,
         agemarr = v511,
         agefbirth = v212,
         nchild = v218,
         marr_fbirth = v221,
         marst = v501,
         husband_educ = v701,
         husband_ind = v704,
         husband_indgen = v705,
         husband_age = v730,
         work = v714,
         wt = v005,
         ind = v716,
         indgen = v717) %>% 
  mutate(
    urban = ifelse(urban == 1, 1, 0),
    u16 = ifelse(agemarr < 16, 1, 0),
    u18 = ifelse(agemarr < 18, 1, 0),
    husband_agri = ifelse(husband_indgen == 4, 1, 0)
  ) %>% 
  left_join(dhs06_hh) %>% 
  group_by(caseid, cluster, hh) %>% 
  mutate(hhid = cur_group_id()) %>% 
  group_by(caseid, cluster, hh, iv) %>% 
  mutate(ivid = cur_group_id(),
         year = 2006) %>% 
  ungroup() %>% 
  select(year, caseid, cluster, regionid, districtid, urban, hh, iv, hhid, ivid, monthint, yrint, birthda, birthmo, birthyr, age, yrschool, educattain, marst, u16, u18,
         marr_fbirth, chborn, agemarr, agefbirth, nchild, work, ind, indgen, husband_age, husband_educ, husband_ind, husband_indgen, husband_agri, agri_land, livestock, wealthind, wt, hhwt)

# 2012 

dhs12_hh <- dhs_hh_12 %>% 
  clean_dhs_hh_fn()

dhs12 <- dhs_w_12 %>% 
  clean_dhs_w_fn() %>% 
  left_join(dhs12_hh) %>% 
  group_by(caseid, cluster, hh) %>% 
  mutate(hhid = cur_group_id()) %>% 
  group_by(caseid, cluster, hh, iv) %>% 
  mutate(ivid = cur_group_id(),
         year = 2012) %>% 
  ungroup() %>% 
  select(year, caseid, cluster, regionid, districtid, urban, hh, iv, hhid, ivid, monthint, yrint, birthda, birthmo, birthyr, age, yrschool, educattain, marst, u16, u18,
         marr_fbirth, chborn, agemarr, agefbirth, nchild, work, ind, indgen, dm1, dm2, dm3, dm4, dm_index, dv1, dv2, dv3, dv4, dv5, dv_index,
         husband_age, husband_educ, husband_ind, husband_indgen, husband_agri, agri_land, livestock, wealthind, wt, dv_wt, hhwt)

# 2017 

dhs17_hh <- dhs_hh_17 %>% 
  clean_dhs_hh_fn()

dhs17 <- dhs_w_17 %>% 
  clean_dhs_w_fn() %>% 
  left_join(dhs17_hh) %>% 
  group_by(caseid, cluster, hh) %>% 
  mutate(hhid = cur_group_id()) %>% 
  group_by(caseid, cluster, hh, iv) %>% 
  mutate(ivid = cur_group_id(),
         year = 2017) %>% 
  ungroup() %>% 
  select(year, caseid, cluster, regionid, districtid, urban, hh, iv, hhid, ivid, monthint, yrint, birthda, birthmo, birthyr, age, yrschool, educattain, marst, u16, u18,
         marr_fbirth, chborn, agemarr, agefbirth, nchild, work, ind, indgen, dm1, dm2, dm3, dm4, dm_index, dv1, dv2, dv3, dv4, dv5, dv_index,
         ipv1, ipv2, ipv3, ipv4, ipv5, ipv6, ipv7, ipv8, ipv9, ipv10, ipv11, ipv12, ipv13, any_viol, less_viol, severe_viol, sexual_viol,
         husband_age, husband_educ, husband_ind, husband_indgen, husband_agri, agri_land, livestock, wealthind, wt, dv_wt, hhwt)

# Combining all
dhs <- bind_rows(dhs06, dhs12, dhs17)

# Creating SHP 
dhs$district <- as_factor(dhs$districtid)
dhs$region <- as_factor(dhs$regionid)

dhs_dist_06_shp <- dhs_dist_06_shp %>%
  select(DHSCLUST, geometry) %>% 
  rename(cluster = DHSCLUST) %>% 
  st_make_valid()
dhs_dist_17_shp <- dhs_dist_17_shp %>%
  select(DHSCLUST, geometry) %>% 
  rename(cluster = DHSCLUST) %>% 
  st_make_valid()
dhs_dist_06_shp <- st_transform(dhs_dist_06_shp, st_crs(dist_pk))
dhs_dist_17_shp <- st_transform(dhs_dist_17_shp, st_crs(dist_pk))
dist_pk  <- st_make_valid(dist_pk) %>% select(PROVINCE, DISTRICT, geometry)
dhs_dist_06_shp <- st_join(dhs_dist_06_shp, dist_pk)
dhs_dist_17_shp <- st_join(dhs_dist_17_shp, dist_pk)
  
dhs06_shp <- left_join(dhs06, dhs_dist_06_shp)
dhs17_shp <- left_join(dhs06, dhs_dist_17_shp)

dhs0617 <- bind_rows(dhs06_shp, dhs17_shp)

dhs0617_sum <- dhs0617 %>% 
  

dhs_sum_fn <- function(i){
  i %>% 
    left_join(dhs_dist) %>% 
    mutate(district_recode = toupper(as.character(district_recode)),
           region_recode = toupper(as.character(region_recode)),
           district_recode = ifelse(district == "Mandi Bahauddin", "MANDI BAHAUDDIN", district_recode),
           district_recode = case_when(
             district_recode == "JAFARABAD" ~ "JAFFARABAD",
             district_recode == "KACHHI (BOLAN)" ~ "KACHHI",
             district_recode == "KECH (TURBAT)" ~ "KECH",
             district_recode == "SHERANI" ~ "SHEERANI",
             district_recode == "NORTH WAZIRISTAN AGENCY" ~ "N. WAZIRASTAN",
             district_recode == "SOUTH WAZIRISTAN AGENCY" ~ "S. WAZIRASTAN",
             district_recode == "D. I. KHAN" ~ "D I KHAN",
             district_recode == "TOR GHAR" ~ "TORDHER",
             district_recode == "DERA GHAZI KHAN" ~ "D G KHAN",
             district_recode == "MUZAFFARGARH" ~ "MUZAFARGARH",
             district_recode == "TOBA TEK SINGH" ~ "T. T SINGH",
             district_recode == "KAMBAR SHAHDADKOT" ~ "SHAHDAD KOT",
             district_recode == "KARACHI MALIR" ~ "MALIR",
             district_recode == "NAUSHAHRO FIROZE" ~ "NAUSHAHRO FEROZ",
             district_recode == "RAHIM YAR KHAN" ~ "R Y KHAN",
             district_recode == "SHAHEED BENAZIRABAD" ~ "S. BENAZIRABAD",
             district_recode == "SHIKARPUR" ~ "SHIKARPHUR",
             district_recode == "SUJAWAL" ~ "SUJJAWAL",
             district_recode == "TANDO ALLAHYAR" ~ "T. AYAR",
             district_recode == "TANDO MUHAMMAD KHAN" ~ "T. M KHAN",
             TRUE ~ district_recode),
           region_recode = case_when(
             region_recode == "GILGIT BALTISTAN" ~ "GB",
             region_recode == "ICT" ~ "FEDERAL CAPITAL TERRITORY",
             region_recode == "KPK" ~ "KHYBER PAKHTUNKHWA",
             TRUE ~ region_recode
           )) %>% 
    group_by(region_recode, district_recode) %>% 
    summarise(
      u16 = weighted.mean(u16, wt, na.rm = T),
      u18 = weighted.mean(u18, wt, na.rm = T),
      agefbirth = weighted.mean(agefbirth, wt, na.rm = T),
      agemarr = weighted.mean(agemarr, wt),
      chborn = weighted.mean(chborn, wt),
      nchild = weighted.mean(nchild, wt),
      work = weighted.mean(work, wt, na.rm = T),
      yrschool = weighted.mean(yrschool, wt),
      less_viol = weighted.mean(less_viol, dv_wt, na.rm = T),
      severe_viol = weighted.mean(severe_viol, dv_wt, na.rm = T),
      sexual_viol = weighted.mean(sexual_viol, dv_wt, na.rm = T)
    ) 
}

dhs_dist_sum <- dhs17 %>% 
  dhs_sum_fn()

dhs_dist_2024_sum <- dhs17 %>% 
  filter(age > 19 & age < 25) %>% 
  dhs_sum_fn()

dist_pk_shp <- dist_pk %>%
  rename(region_recode = PROVINCE, district_recode = DISTRICT) %>%
  select(region_recode, district_recode, geometry) %>% 
  st_make_valid()

dhs_dist_sum_shp <- dist_pk_shp %>%
  full_join(dhs_dist_sum, by = c("region_recode", "district_recode")) %>% 
  filter(!is.na(region_recode))

dhs_dist_2024_sum_shp <- dist_pk_shp %>%
  full_join(dhs_dist_2024_sum, by = c("region_recode", "district_recode")) %>% 
  filter(!is.na(region_recode))


