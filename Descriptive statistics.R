# DHS 

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = u16 * 100)) +
  scale_fill_viridis_c(direction = -1, name = "Share first married under 16 (%)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/u16.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_2024_sum_shp) +
  geom_sf(aes(fill = u16 * 100)) +
  scale_fill_viridis_c(direction = -1, name = "Share of women aged 20-24 \nfirst married under 16 (%)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/u16_20204.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = u18*100)) +
  scale_fill_viridis_c(direction = -1, name = "Share first married under 18 (%)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/u18.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_2024_sum_shp) +
  geom_sf(aes(fill = u18*100)) +
  scale_fill_viridis_c(direction = -1, name = "Share of women aged 20-24 \nfirst married under 18 (%)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/u18_2024.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = agemarr)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. age of first marriage") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/agemarr.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_2024_sum_shp) +
  geom_sf(aes(fill = agemarr)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. age of first marriage \namong women aged 20-24") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/agemarr_2024.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = agefbirth)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. age of first birth") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/agefbirth.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_2024_sum_shp) +
  geom_sf(aes(fill = agefbirth)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. age of first birth \namong women aged 20-24") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/agefbirth_2024.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = chborn)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. number of children ever born") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/chborn.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_2024_sum_shp) +
  geom_sf(aes(fill = chborn)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. number of children ever born \namong women aged 20-24") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/chborn_2024.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_sum_shp) +
  geom_sf(aes(fill = yrschool)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. years of school completed") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/yrschool.jpeg", width = 14, height = 14)

ggplot(data = dhs_dist_2024_sum_shp) +
  geom_sf(aes(fill = yrschool)) +
  scale_fill_viridis_c(direction = -1, name = "Avg. years of school completed \namong women aged 20-24") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )
ggsave("Figures/yrschool_2024.jpeg", width = 14, height = 14)
