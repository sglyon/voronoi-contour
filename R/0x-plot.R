
pnt <-
  config$path_pnt %>%
  teproj::import_path_cleanly()
pnt


z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
spdf_base_map0 <- readRDS(z)
spdf_base_map0
spdf_base_map <- sp::spTransform(spdf_base_map0, sp::CRS("+proj=longlat +datum=WGS84"))

spdf <-
  sp::SpatialPointsDataFrame(
    cbind(pnt$lng, pnt$lat),
    pnt,
    match.ID = TRUE
  )
spdf
spdf@bbox <- spdf_base_map@bbox
sp::proj4string(spdf) <- sp::proj4string(spdf_base_map)
spdf

spdf %>% maptools::as.ppp.SpatialPointsDataFrame()

th <-
  spdf %>% 
  maptools::as.ppp.SpatialPointsDataFrame() %>% 
  spatstat::dirichlet() %>% 
  methods::as("SpatialPolygons")
sp::proj4string(th) <- sp::proj4string(spdf)
th_z <- sp::over(th, spdf)
spdf_tess <- sp::SpatialPolygonsDataFrame(th, th_z)
spdf_tess
spdf_tess_trimmed <- raster::intersect(spdf_base_map, spdf_tess)
spdf_tess_trimmed
# tmap::tm_shape(spdf_tess_trimmed) + tmap::tm_polygons()

tx_border <-
  teplot::get_map_data_state(state = "tx") %>%
  as_tibble()

sf_tess_trimmed <- spdf_tess_trimmed %>% sf::st_as_sf()
viz_pnt <-
  ggplot() +
  geom_polygon(
    data = tx_border,
    aes(x = long, y = lat, group = group),
    size = 1.5,
    color = "black",
    fill = NA
  ) +
  geom_sf(
    data = sf_tess_trimmed,
    aes(fill = value)
  ) +
  coord_sf(datum = NA) +
  scale_color_viridis_c(option = "E", na.value = "#FFFFFF") +
  teplot::theme_map(legend.position = "bottom") +
  labs(
    title = str_wrap("ERCOT, Real-Time LMP Prices", 80),
    caption = "By Tony ElHabr."
  )
viz_pnt

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
ggplot(nc) +
  geom_sf(aes(fill = AREA))
# 
# ggplot() +
#   geom_polygon(
#     data = tx_border,
#     aes(x = long, y = lat, group = group),
#     size = 1.5,
#     color = "black",
#     fill = NA
#   )