id <- c("division", "HH7", "HH6", "HH1", "HH2", "HL1", "windex5")

marriage_fn <- function(i){
  
  i %>% 
    select(HH7, division, HH6, HH1, HH2, HL1, HH7, WB3M, WB3Y, WB4, WB5, WB6A, MA1, MA2, MA3, MA8M, MA8Y,
           MA11, DV1A, DV1B, DV1C, DV1D, DV1E, VT1, windex5, wmweight) %>% 
    mutate(
      WB3M = ifelse(WB3M == 98, NA, WB3M),
      MA8Y = ifelse(MA8Y == 9998 | MA8Y == 9997, NA, MA8Y),
      MA8M = ifelse(MA8M == 98 | MA8M == 97, NA, MA8M),
      birth_dec = WB3Y + ((WB3M - 1) / 12),
      marriage_dec = MA8Y + ((MA8M - 1) / 12),
      first_marriage = floor(marriage_dec - birth_dec),
      MA11 = ifelse(is.na(MA11) & MA1 == 1, first_marriage, MA11),
      MA11 = ifelse(is.na(MA11) & MA1 == 1, MA8Y - WB3Y, MA11),
      MA11 = ifelse(MA11 < 0 | MA11 > 96, NA, MA11),
      division = as_factor(division),
      HH7 = as_factor(HH7),
      HH6 = as_factor(HH6)
    )
}

balochistan_wm <- balochistan_wm %>% rename(HL1 = LN)
kp_wm <- kp_wm %>% rename(HL1 = LN)
punjab_wm <- punjab_wm %>% rename(HL1 = LN, HH7 = hh7)
sindh_wm <- sindh_wm %>% rename(HL1 = LN)

punjab_hl <- punjab_hl %>% rename(HH7 = hh7)

balochistan_wmn <- merge(balochistan_wm, balochistan_hl, by = id) %>% 
  marriage_fn() %>% 
  mutate(province ="Balochistan",
         HH7 = case_when(
           HH7 == "Chaghi" ~ "Chagai",
           HH7 == "Kachhi (Bolan)" ~ "Kachhi",
           HH7 == "Kech (Turbat)" ~ "Kech",
           HH7 == "Sibbi" ~ "Sibi",
           HH7 == "Sheerani" ~ "Sherani",
           HH7 == "SohbatPur" ~ "Sohbatpur",
           TRUE ~ HH7),
         division = case_when(
           division == "Mekran" ~ "Makran",
           TRUE ~ division
         ))

kp_wmn <- merge(kp_wm, kp_hl, by = id) %>% 
  marriage_fn() %>% 
  mutate(province ="Khyber Pakhtunkhwa",
         HH7 = case_when(
           HH7 == "Abbotabad" ~ "Abbottabad",
           HH7 == "Bajor" ~ "Bajaur",
           HH7 == "Battagram" ~ "Batagram",
           HH7 == "Hari Pur" ~ "Haripur",
           HH7 == "Laki Marwat" ~ "Lakki Marwat",
           HH7 == "Nowshehra" ~ "Nowshera",
           HH7 == "Dera Ismail Khan" ~ "D. I. Khan",
           HH7 == "Kuram" ~ "Kurram",
           HH7 == "Mohmind" ~ "Mohmand",
           HH7 == "Torghar" ~ "Tor Ghar",
           TRUE ~ HH7),
         division = case_when(
           division == "D.I. Khan" ~ "Dera Ismail Khan",
           TRUE ~ division
         ))
punjab_wmn <- merge(punjab_wm, punjab_hl, by = id) %>% 
  marriage_fn() %>% 
  mutate(province ="Punjab",
         HH7 = case_when(
           HH7 == "Attok" ~ "Attock",
           HH7 == "DG Khan" ~ "Dera Ghazi Khan",
           HH7 == "Layyah" ~ "Leiah",
           HH7 == "RY Khan" ~ "Rahim Yar Khan",
           HH7 == "TT Singh" ~ "Toba Tek Singh",
           TRUE ~ HH7),
         division = case_when(
           division == "DG Khan" ~ "Dera Ghazi Khan",
           TRUE ~ division))
sindh_wmn <- merge(sindh_wm, sindh_hl, by = id) %>% 
  marriage_fn() %>% 
  mutate(province ="Sindh",
         HH7 = case_when(
           HH7 == "Karachi Central" ~ "Central Karachi",
           HH7 == "Karachi East" ~ "East Karachi",
           HH7 == "Karachi West" ~ "West Karachi",
           HH7 == "Karachi South" ~ "South Karachi",
           HH7 == "MirpurK Khas" ~ "Mirpur Khas",
           HH7 == "Karachi Korangi" ~ "Korangi Karachi",
           HH7 == "Karachi Malir" ~ "Malir Karachi",
           HH7 == "Shahdad Kot" ~ "Kambar Shahdad Kot",
           HH7 == "Shaheed Benazirabad" ~ "Shaheed Benazir Abad",
           HH7 == "Tando Muhmmad Khan" ~ "Tando Muhammad Khan",
           TRUE ~ HH7))

pkmap3 <- dist_shp %>% 
  rename(province = ADM1_EN,
         HH7 = ADM2_EN) %>% 
  select(province, HH7, geometry) %>% 
  distinct()

chitral <- pkmap3 %>% 
  filter(HH7 %in% c("Chitral Lower", "Chitral Upper")) %>% 
  st_union() %>% 
  st_cast("MULTIPOLYGON")
chitral_sf <- st_sf(
  HH7 = "Chitral",
  geometry = chitral) %>% 
  mutate(province = "Khyber Pakhtunkhwa")

kohistan <- pkmap3 %>% 
  filter(HH7 %in% c("Kohistan Lower", "Kohistan Upper", "Kolai Palas Kohistan")) %>% 
  st_union() %>% 
  st_cast("MULTIPOLYGON")
kohistan_sf <- st_sf(
  HH7 = "Kohistan",
  geometry = kohistan) %>% 
  mutate(province = "Khyber Pakhtunkhwa")

pkmap_clean <- pkmap3 %>%
  filter(!HH7 %in% c("Kohistan Lower", "Kohistan Upper", "Kolai Palas Kohistan", "Chitral Lower", "Chitral Upper"))
pkmap_clean <- bind_rows(pkmap_clean, chitral_sf, kohistan_sf)

marriagedata_mics <- bind_rows(balochistan_wmn, kp_wmn, punjab_wmn, sindh_wmn)

marriage_sum <- marriagedata_mics %>% 
  filter(WB4 > 19 & WB4 < 25) %>% 
  group_by(province, HH7, division) %>% 
  summarise(
    n = sum(wmweight),
    n_married = sum(wmweight * (MA1 == 1), na.rm = T),
    age_marr = weighted.mean(MA11, wmweight, na.rm = T),
    u18_marr_n = sum(wmweight * (MA11 < 18), na.rm = T)
  ) %>% 
  mutate(share_u18_marr = u18_marr_n/n_married) %>% 
  select(province, division, HH7, everything())

u18_marr_shp <- full_join(pkmap_clean, marriage_sum)

ggplot(u18_marr_shp) + 
  geom_sf(aes(fill = share_u18_marr*100)) +
  scale_fill_gradient(name = "Share of women aged 20-24 married \nbefore the age of 18 (%)", low = "green", high = "red", na.value = "grey") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("")
