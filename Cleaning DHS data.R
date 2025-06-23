names(dhs_17) <- make.names(names(dhs_17), unique = TRUE)
names(dhs_17) <- gsub("\\$", "_", names(dhs_17))  
names(dhs_17) <- gsub("[^a-zA-Z0-9_]", "_", names(dhs_17))  
names(dhs_17) <- tolower(names(dhs_17))

dhs17 <- dhs_17 %>% 
  rename(district = sdist,
         hhid = v002,
         ivid = v003,
         monthint = v006,
         yrint = v007,
         birthda = v011,
         birthmo = v009,
         birthyr = v010,
         region = v024,
         urban = v025,
         age = v012,
         educattain = v106,
         yrschool = v133,
         hhsize = v138,
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
    dm1_recode = ifelse(dm1 < 3, 1, 0),
    dm2_recode = ifelse(dm2 < 3, 1, 0),
    dm3_recode = ifelse(dm3 < 3, 1, 0),
    dm4_recode = ifelse(dm4 < 3, 1, 0),
    dm_index = dm1_recode + dm2_recode + dm3_recode + dm4_recode,
    dv1_recode = ifelse(dv1 == 1, 1, 0),
    dv2_recode = ifelse(dv2 == 1, 1, 0),
    dv3_recode = ifelse(dv3 == 1, 1, 0),
    dv4_recode = ifelse(dv4 == 1, 1, 0),
    dv5_recode = ifelse(dv5 == 1, 1, 0),
    dv_index = dv1_recode + dv2_recode + dv3_recode + dv4_recode + dv5_recode,
    u16 = ifelse(agemarr < 16, 1, 0),
    u18 = ifelse(agemarr < 18, 1, 0)
  ) %>% 
  select(region, district, urban, hhid, ivid, monthint, yrint, birthda, birthmo, birthyr, age, yrschool, educattain, marst, u16, u18,
         marr_fbirth, chborn, agemarr, agefbirth, nchild, work, ind, indgen, dm1, dm2, dm3, dm4, dm_index, dv1, dv2, dv3, dv4, dv5, dv_index,
         ipv1, ipv2, ipv3, ipv4, ipv5, ipv6, ipv7, ipv8, ipv9, ipv10, ipv11, ipv12, ipv13, less_viol, severe_viol, sexual_viol,
         husband_age, husband_educ, husband_ind, husband_indgen, hhsize, wealthind, wt, dv_wt)

dhs_sum <- dhs17 %>% 
  group_by(region) %>% 
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

dhs_shp <- dhs_shp %>% rename(region = DHSCODE)

dhs_sum_shp <- left_join(dhs_sum, dhs_shp) %>% st_as_sf()

ggplot(data = dhs_sum_shp) +
  geom_sf(aes(fill = u16)) +
  scale_fill_viridis_c(name = "Share married under \nthe age of 16", labels = scales::percent) +
  theme_minimal()
