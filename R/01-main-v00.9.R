
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
  
  lmp_pnt %>% teproj::export_path(config$path_lmp_pnt)
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

# spdf_tess_trim ----
.cnd <- (file.exists(config$path_sf_pnt) & file.exists(config$path_sf_tess_trim))
if(.cnd) {
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
  th_z <- sp::over(th, spdf_pnt)
  spdf_tess <- sp::SpatialPolygonsDataFrame(th, th_z)
  spdf_tess_trim <- raster::intersect(spdf_base_map, spdf_tess)
  sf_tess_trim <- spdf_tess_trim %>% sf::st_as_sf()
  # sf_tess_trim %>% teproj::export_path(config$path_sf_tess_trim)
  sf_tess_trim %>% readr::write_rds(config$path_sf_tess_trim)
}

