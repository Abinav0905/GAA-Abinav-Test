install.packages("pacman")
pacman::p_load(sf, tidyverse)
mpsz = st_read("Hands_on_Ex/Hands_onEx01/data/geospatial/MasterPlan2014SubzoneBoundaryNoSea.geojson")

cyclingpath = st_read("Hands_on_Ex/Hands_onEx01/data/geospatial/CyclingPathGazette.shp")

preschool = st_read("Hands_on_Ex/Hands_onEx01/data/geospatial/PreSchoolsLocation.geojson")

st_geometry(mpsz)
glimpse(mpsz)
head(mpsz, n = 5)
plot(mpsz)
plot(st_geometry(mpsz))
plot(mpsz["PLN_AREA_N"])
plot(st_geometry(mpsz))
plot(st_geometry(preschool), add = TRUE)

st_crs(mpsz)
st_crs(preschool)

mpsz = st_transform(mpsz, crs = 3414)
preschool = st_transform(preschool, crs = 3414)
st_crs(cyclingpath)
cyclingpath = st_set_crs(cyclingpath, 3414)


plot(st_geometry(mpsz))
plot(st_geometry(preschool), add = TRUE)


listings = read_csv("Hands_on_Ex/Hands_onEx01/data/aspatial/listings.csv")
list(listings)

listings_sf <- st_as_sf(listings, 
                       coords = c("longitude", "latitude"),
                       crs = 4326) %>%
  st_transform(crs = 3414)

glimpse(listings_sf)

plot(st_geometry(mpsz))
plot(st_geometry(listings_sf), add = TRUE)

buffer_cycling = st_buffer(cyclingpath, dist = 5, nQuadSegs = 30)

buffer_cycling = buffer_cycling %>%
  mutate(AREA = st_area(geometry))

sum(buffer_cycling$AREA)

mpsz_selected = mpsz %>%
  filter(SUBZONE_N == "TAMPINES WEST")

buffer_cycling_selected = st_intersection(buffer_cycling, mpsz_selected)

plot(buffer_cycling_selected)

mpsz$`PreSch Count` = lengths(st_intersects(mpsz, preschool))

summary(mpsz$`PreSch Count`)

top_n(mpsz, 1, `PreSch Count`)

mpsz$Area = mpsz %>%
  st_area()

mpsz = mpsz %>%
  mutate(`PreSch Density` = `PreSch Count`/Area * 1000000)

ggplot(data = mpsz, 
       aes(x = as.numeric(`PreSch Density`))) +
  geom_histogram(bins = 20, 
                 color = "black", 
                 fill = "light blue") +
  labs(title = "Are pre-school even distributed in Singapore?",
       subtitle = "There are many planning subzones with a single pre-school, on the other hand, \nthere are seven planning subzones with at least 30 or more pre-schools",
       x = "Pre-school density (per km sq)",
       y = "Frequency")

ggplot(data = mpsz, 
       aes(y = `PreSch Count`, 
           x = as.numeric(`PreSch Density`))) +
  geom_point(color = "black", 
             fill = "light blue") +
  xlim(0, 40) +
  ylim(0, 40) +
  labs(title = "",
       x = "Pre-school density (per km sq)",
       y = "Pre-school count")


mpsz = st_read("Hands_on_Ex/Hands_onEx01/data/geospatial/MasterPlan2019PlanningAreaBoundaryNoSea.geojson") %>%
  st_transform(crs = 3414)

glimpse(mpsz)

