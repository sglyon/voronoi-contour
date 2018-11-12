
if(!file.exists(config$path_png_map_raw)) {
  url <- "http://www.ercot.com/content/cdr/contours/rtmLmp.png?uniquenessFactor=1"
  download.file(url = url, destfile = config$path_png_map_raw, silent = TRUE)
}
if(!file.exists(config$path_png_legend_raw)) {
  url <- "http://www.ercot.com/content/cdr/contours/rtmLmpLegend.png?uniquenessFactor=1"
  download.file(url = url, destfile = config$path_png_legend_raw, silent = TRUE)
}

# kml_pnt ----
if(file.exists(config$path_kml_pnt)) {
  kml_pnt <-
    config$path_kml_pnt %>% 
    teproj::import_path_cleanly()
} else {
  kml_pnt_raw <-
    config$path_kml_raw %>%
    tidykml::kml_points()
  
  kml_pnt <-
    kml_pnt_raw %>%
    select(name, lat = latitude, lng = longitude) %>%
    distinct(name, lat, lng) %>%
    arrange(name)
  kml_pnt
  kml_pnt %>% teproj::export_path(config$path_kml_pnt)
}

# color ----
if(file.exists(config$path_color)) {
  color <-
    config$path_color %>% 
    teproj::import_path_cleanly()
} else {
  xml_legend <-
    config$path_kml_raw %>%
    xml2::read_xml() %>%
    xml2::xml_children() %>%
    xml2::xml_children() %>%
    magrittr::extract(3:36)
  
  legend_color <-
    xml_legend %>% 
    xml2::xml_text() %>% 
    str_replace_all("^0CA", "") %>% 
    str_replace_all("([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})", "\\3\\2\\1") %>% 
    paste0("#", .)
  legend_color
  
  values_color <-
    c(
      -250L,
      -100L,
      -50L,
      -40L,
      -30L,
      -20L,
      -10L,
      0L,
      10L,
      20L,
      30L,
      40L,
      50L,
      60L,
      70L,
      80L,
      90L,
      100L,
      110L,
      120L,
      130L,
      140L,
      150L,
      160L,
      170L,
      180L,
      190L,
      200L,
      250L,
      500L,
      1000L,
      2000L,
      3000L,
      9000L
    )
  color <-
    tibble(
      hex = legend_color,
      value = values_color
    )
  color
  color %>% teproj::export_path(config$path_color)
}

# lmp_pnt ----
if(file.exists(config$path_lmp_pnt)) {
  lmp_pnt <-
    config$path_lmp_pnt %>% 
    teproj::import_path_cleanly()
} else {
  url_lmp <- "http://www.ercot.com/content/cdr/html/current_np6788"
  html_lmp <-
    url_lmp %>%
    xml2::read_html()
  
  table_raw <-
    html_lmp %>%
    # rvest::html_table(fill = TRUE)
    rvest::html_nodes("table") %>% 
    purrr::pluck(1) %>% 
    rvest::html_table(fill = TRUE) %>% 
    as_tibble() %>% 
    select(1:5)
  nms_raw <-
    table_raw %>%
    slice(3) %>% 
    t() %>% 
    c()
  lmp_pnt_raw <-
    table_raw %>% 
    slice(5:n()) %>% 
    purrr::set_names(nms_raw)
  lmp_pnt <-
    lmp_pnt_raw %>%
    select(1:2) %>% 
    purrr::set_names(c("name", "lmp")) %>% 
    mutate_at(vars(lmp), funs(as.numeric))

  lmp_pnt %>% teproj::export_path(config$path_lmp)
}

# pnt ----
if(file.exists(config$path_pnt)) {
  pnt <-
    config$path_pnt %>% 
    teproj::import_path_cleanly()
} else {
  pnt <-
    kml_pnt %>%
    inner_join(lmp_pnt)
  
  pnt <-
    pnt %>%
    group_by(lng, lat) %>%
    summarise_at(vars(lmp), funs(max)) %>%
    ungroup() %>% 
    inner_join(pnt) %>% 
    group_by(lng, lat) %>% 
    filter(row_number() == 1L) %>% 
    ungroup()
  # pnt %>% 
  #   rename(lmp = value) %>% 
  #   fuzzyjoin::fuzzy_left_join(
  #     color %>% mutate_at(vars(value), funs(as.double)),
  #     by = c("lmp" = "value", "lmp" = "value"),
  #     match_fun = list(`<=`, `>`)
  #   )
  pnt <-
    pnt %>%
    mutate(dummy = 1L) %>% 
    left_join(color %>% mutate(dummy = 1L), by = "dummy") %>% 
    filter(lmp <= value) %>% 
    select(-dummy) %>% 
    group_by(name, lng, lat) %>% 
    arrange(value, .by_group = TRUE) %>% 
    filter(row_number() == 1L) %>% 
    ungroup() %>% 
    select(lng, lat, value, name)
  pnt %>% teproj::export_path(config$path_pnt)
}

# spdf_base_map ----
if(file.exists(config$path_spdf_base_map)) {
  spdf_base_map <-
    config$path_spdf_tess_trim %>% 
    teproj::import_path_cleanly()
} else {
  z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
  spdf_base_map <- readRDS(z)
  spdf_base_map <- sp::spTransform(spdf_base_map, sp::CRS("+proj=longlat +datum=WGS84"))
  # spdf_base_map %>% teproj::export_path(config$path_spdf_base_map)
  spdf_base_map %>% readr::write_rds(config$path_spdf_base_map)
}

# spdf_tess_trim ----
if(file.exists(config$path_sf_pnt) & file.exists(config$path_sf_tess_trim)) {
  sf_pnt <-
    config$path_sf_pnt %>% 
    teproj::import_path_cleanly()
  sf_tess_trim <-
    config$path_sf_tess_trim %>% 
    teproj::import_path_cleanly()
} else {
  spdf_pnt <-
    sp::SpatialPointsDataFrame(
      cbind(pnt$lng, pnt$lat),
      pnt,
      match.ID = TRUE
    )
  spdf_pnt@bbox <- spdf_base_map@bbox
  sp::proj4string(spdf_pnt) <- sp::proj4string(spdf_base_map)
  sf_pnt <- spdf_pnt %>% sf::st_as_sf()
  sf_pnt %>% readr::write_rds(config$path_sf_pnt)
  
  th <-
    spdf_pnt %>% 
    maptools::as.ppp.SpatialPointsDataFrame() %>% 
    spatstat::dirichlet() %>% 
    methods::as("SpatialPolygons")
  sp::proj4string(th) <- sp::proj4string(spdf_pnt)
  th_z <- sp::over(th, spdf)
  spdf_tess <- sp::SpatialPolygonsDataFrame(th, th_z)
  spdf_tess_trim <- raster::intersect(spdf_base_map, spdf_tess)
  sf_tess_trim <- spdf_tess_trim %>% sf::st_as_sf()
  # sf_tess_trim %>% teproj::export_path(config$path_sf_tess_trim)
  sf_tess_trim %>% readr::write_rds(config$path_sf_tess_trim)
}

# viz_pnt ----
tx_border <-
  teplot::get_map_data_state(state = "tx") %>%
  as_tibble()
tx_county <-
  teplot::get_map_data_county(state = "tx") %>% 
  as_tibble()

labs_fill <-
  color$value %>%
  scales::dollar() %>% 
  paste0("<" , .)

viz_pnt <-
  ggplot() +
  geom_sf(
    data =
      sf_tess_trim %>% 
      mutate_at(vars(value), funs(factor(., levels = color$value))),
    aes(fill = value),
    color = NA
  ) +
  scale_fill_manual(
    values = color$hex, 
    labels = labs_fill,
    drop = FALSE
  ) +
  guides(
    fill = guide_legend(ncol = 1, reverse = TRUE)
  ) +
  # geom_polygon(
  #   data = tx_border,
  #   aes(x = long, y = lat, group = group),
  #   size = 1,
  #   color = "black",
  #   fill = NA
  # ) +
  geom_polygon(
    data = tx_county,
    aes(x = long, y = lat, group = group),
    size = 0.1,
    color = "black",
    fill = NA
  ) +
  geom_sf(
    data = sf_pnt,
    shape = 21,
    size = 2,
    stroke = 1,
    color = "red",
    fill = "black"
  ) +
  coord_sf(datum = NA) +
  teplot::theme_map() +
  theme(
    panel.background = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = str_wrap("ERCOT, Real-Time LMP Prices", 80),
    caption = "By Tony ElHabr."
  )
viz_pnt
