library(tidyverse)
library(nycflights13)
airports_lat_long = airports %>% select(lat, lon, faa)
routes = flights %>% 
  mutate(flight_id = seq_len(nrow(.))) %>% # add surrougate key
  left_join(airports_lat_long, by = c(origin = 'faa')) %>%
  left_join(airports_lat_long, by = c(dest = 'faa'), suffix = c('.origin', '.dest')) %>% 
  slice_sample(n = 500) %>% #Take a reasonable subsample so the plot isn't illegible
  select(flight_id, lon.dest, lat.dest, lon.origin, lat.origin) # Take only the columns we need

## Initial attempt, didn't manage to show routing
ggplot(routes_wide, aes(x = lon, y = lat, shape =source)) +
  geom_point() + geom_line(aes(group = flight_id))


routes_long = pivot_longer(routes, -flight_id) %>% 
  mutate(source = 
           ifelse(name %in% c('lat.origin', 'lon.origin'),
                  'origin', 'dest'), #extract types of observation from column name
         variable = ifelse(name %in% c('lat.origin', 'lat.dest'), 'lat', 'lon')) 

routes_wide = routes_long %>% 
  pivot_wider(id_cols = c(flight_id, source), # Each row should be the combination of flight_id and source
              names_from = variable)

plt = ggplot(routes_wide, aes(x = lon, y = lat, shape =source)) +
  geom_point() + geom_line(aes(group = flight_id))

# plot everything
plt

# plot zoomed on North East
plt + coord_cartesian(xlim = c(-90, -70), ylim = c(37, 42))
