# Load up the map requiring the latitude and longitude
library(tidyverse)
library(leaflet)
library(mapview)
library(FLUXNETGranger)
max_lag <- 14  # 14 day lag

flux_data_sites <- la_thuille_small %>%
  relocate(time) %>%
  group_by(site) %>%
  nest() %>%
  mutate(enough = map(.x=data,.f=~(value <- .x %>% na.omit %>% NROW() ) )) %>%
  filter(enough > max_lag) %>%
  select(-enough) %>%
  pull(site)

# Filter out on these sites
my_site_info <- la_thuille_site_info %>%
  filter(SITE %in% flux_data_sites)

mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +
  mapWorld +
  geom_point(data=my_site_info,mapping=aes(x=longitude, y=latitude,color=IGBP), size=2) +
  coord_quickmap() +
  labs(x=expression("Longitude " ( degree)), y=expression("Latitude " ( degree))) +
  xlim(c(-180,180)) +
  theme_bw() +
  theme(legend.position = "bottom")
