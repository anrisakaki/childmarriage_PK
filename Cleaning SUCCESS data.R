succ <- succ %>% 
  mutate(
    u16_marriage = ifelse(b1 == 2 & b7 < 16, 1, 0),
    u18_marriage = ifelse(b1 == 2 & b7 < 18, 1, 0),
    u16_marriage = ifelse(is.na(u16_marriage), 0, u16_marriage),
    u18_marriage = ifelse(is.na(u18_marriage), 0, u18_marriage),
    married = ifelse(!is.na(b7), 1, 0)
  )

sum1 <- succ %>% 
  filter(b1 == 2 & b4 < 25) %>% 
  group_by(b4) %>% 
  summarise(
    u25_girls_n = n(),
    n_married = sum(married == 1, na.rm = T),
    mean_age_marriage = mean(b7, na.rm = T)
    ) %>% 
  mutate(share_married = (n_married/u25_girls_n)*100)

sum2 <- succ %>% 
  filter(b1 == 2 & b4 < 25 & b4 > 19) %>% 
  group_by(PSC) %>% 
  summarise(
    n = n(),
    u18_marriage = sum(u18_marriage == 1),
    mean_age_marriage = mean(b7, na.rm = T)
  ) %>% 
  mutate(share_married = (u18_marriage/n)*100)

