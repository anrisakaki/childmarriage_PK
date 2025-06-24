# DHS 

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = u16 * 100)) +
  scale_fill_viridis_c(direction = -1, name = "Share first married under 16 (%)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = u18*100)) +
  scale_fill_viridis_c(direction = -1, name = "Share first married under 18 (%)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = chborn)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. child ever born") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = yrschool)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. years of \neducation completed") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
