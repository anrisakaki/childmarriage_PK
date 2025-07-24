# DHS 
ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = (u16_12 - u16_06) * 100)) +
  scale_fill_gradient2(
    low = "blue",     
    mid = "lightyellow",     
    high = "red",     
    midpoint = 0,     
    name = "Change in share first married under 16 \n between 2006 and 2012 (p.p.)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/u16_change_0612.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = (u18_12 - u18_06) * 100)) +
  scale_fill_gradient2(
    low = "blue",     
    mid = "lightyellow",     
    high = "red",     
    midpoint = 0,     
    name = "Change in share first married under 18 \n between 2006 and 2012 (p.p.)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/u18_change_0612.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = agemarr_change)) +
  scale_fill_gradient2(
    low = "blue",     
    mid = "white",     
    high = "red",     
    midpoint = 0,     
    name = "Change in avg. age of first marriage \n between 2006 and 2017"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/agemarr_change.jpeg", width = 14, height = 14)
