library(ggplot2)
library(gganimate)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(sf)

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

# get count of species sighted at each monitor in each month
obs_gps <- feederwatch %>% 
  filter(species_code %in% c('daejun', 'dowwoo', 'norcar')) %>%
  mutate(
    first_of_month = floor_date(ymd(paste(Year, Month, Day)), unit = 'months')
  ) %>%
  right_join( # make sure all months are represented for each monitor X species
    expand.grid( 
      species_code = c('daejun', 'dowwoo', 'norcar'), 
      first_of_month = seq.Date(from = min(.$first_of_month), to = max(.$first_of_month), by = 'months'),
      loc_id = site_data$loc_id
    ),
    by = c('species_code', 'first_of_month', 'loc_id')
  ) %>%
  group_by(loc_id, species_code, first_of_month, latitude, longitude) %>%
  summarize(n_obs = sum(replace_na(how_many, 0))) %>%
  group_by(loc_id, species_code, latitude, longitude) %>%
  filter(sum(n_obs) != 0) %>%
  left_join(
    tribble(
      ~species_code, ~species, 
      'daejun', 'Dark-eyed Junco',
      'dowwoo', 'Downy Woodpecker (Pacific)',
      'norcar', 'Northern Cardinal'
    ), by = 'species_code'
  )%>%
  ungroup() %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)  # Finally, convert to sf


bg <- rnaturalearth::ne_countries(returnclass = 'sf', continent = 'north america') %>%
  filter(iso_a2 %in% c('US', 'CA')) 

vis <- ggplot() + 
  geom_sf(data = bg) + 
  geom_sf(
    data = obs_gps,
    aes(size = n_obs, color = species, group = loc_id),
    alpha = .5
  ) + 
  facet_wrap(~species, nrow = 3) +
  scale_size_continuous(guide = 'none') + 
  scale_color_discrete(guide = 'none') +
  theme_void() + 
  ggtitle('Bird Sightings - Month {frame}') + 
  transition_time(first_of_month)

animate(vis, nframes = length(unique(obs_gps$first_of_month)), fps = 1)
anim_save('~/../Downloads/birbs.gif')
