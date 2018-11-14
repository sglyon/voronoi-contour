
cnd <-
  !(
      file.exists(config$path_png_map_spp_rtm_raw) &
      file.exists(config$path_png_map_spp_dam_raw) &
        file.exists(config$path_png_legend_raw)
  )

if(cnd) {
  downloa_png <- function(url = NULL, path, mkt = c("rtm", "dam"), hour = NULL) {
    if(is.null(url)) {
      mkt <- match.arg(mkt)
      if(mkt == "rtm") {
        hour <- ""
      } else {
        if(is.null(hour)) {
          stop("`hour` cannot be `NULL`.", call. = FALSE)
        }
      }
      url <- sprintf("http://www.ercot.com/content/cdr/contours/%sSpp%s.png?uniquenessFactor=1", mkt, hour)
    }
    download.file(url = url, destfile = path, mode = "wb", quiet = TRUE)
    invisible(path)
  }
  
  download_png(path = config$path_png_map_spp_rtm_raw, mkt = "rtm")
  download_png(path = config$path_png_map_spp_dam_raw, mkt = "dam", hour = 15)
  download_png(
    url = "http://www.ercot.com/content/cdr/contours/rtmLmpLegend.png?uniquenessFactor=1", 
    path = config$path_png_legend_raw
  )
}


# kml_pnt ----
cnd <- file.exists(config$path_kml_pnt)
if(cnd) {
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
cnd <- file.exists(config$path_color)
if(cnd) {
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

# TODO
# spp_pnt ----
cnd <- (file.exists(config$path_pnt_rtm) & file.exists(config$path_pnt_dam))
if(cnd) {
  pnt_rtm <-
    config$path_pnt_rtm %>% 
    teproj::import_path_cleanly()
  pnt_dam <-
    config$path_pnt_dam %>% 
    teproj::import_path_cleanly()
} else {
  cnd <- (file.exists(config$path_spp_rtm_raw) & file.exists(config$path_spp_dam_raw))
  if(cnd) {
    spp_rtm_raw <-
      config$path_spp_rtm_raw %>% 
      teproj::import_path_cleanly()
    
    spp_dam_raw <-
      config$path_spp_dam_raw %>% 
      teproj::import_path_cleanly()
    # spp_rtm_raw %>% count(settlement_point_type)
    spp_rtm <-
      spp_rtm_raw %>% 
      mutate_at(vars(delivery_date), funs(lubridate::mdy)) %>% 
      mutate(
        datetime = 
          paste0(delivery_date, " ", sprintf("%02d", delivery_hour - 1L), ":", sprintf("%02d", (delivery_interval * 15L) %% 60L), ":00")
      ) %>% 
      mutate_at(vars(datetime), funs(lubridate::ymd_hms)) %>% 
      select(datetime, matches("settlement_point")) %>% 
      rename_all(funs(str_replace_all(., "settlement_point_", ""))) %>% 
      filter(type %in% c("RN", "PUN", "PCCRN")) %>% 
      select(datetime, name, spp = price)
    spp_rtm
    datetime_filt <- lubridate::ymd_hms("2018-11-13 07:00:00")
    spp_dam <-
      spp_dam_raw %>%
      mutate_at(vars(hour_ending), funs(as.character)) %>% 
      mutate_at(vars(delivery_date), funs(lubridate::mdy)) %>% 
      mutate_at(vars(delivery_date), funs(if_else(is.na(hour_ending), . + lubridate::days(1), .))) %>% 
      mutate_at(vars(hour_ending), funs(coalesce(., "00:00:00"))) %>% 
      mutate(datetime = paste0(delivery_date, " ", hour_ending) %>% lubridate::ymd_hms()) %>% 
      filter(datetime == datetime_filt) %>% 
      select(datetime, matches("settlement_point")) %>% 
      rename(name = settlement_point, spp = settlement_point_price) %>% 
      select(datetime, name, spp)
    spp_dam
    # kml_pnt %>% anti_join(spp_dam)
    # kml_pnt %>% anti_join(spp_rtm)
    
    add_lng_lat_cols <-
      function(data) {
        pnt <-
          kml_pnt %>%
          inner_join(data)
        
        pnt <-
          pnt %>%
          group_by(lng, lat) %>%
          summarise_at(vars(spp), funs(max)) %>%
          ungroup() %>% 
          inner_join(pnt) %>% 
          group_by(lng, lat) %>% 
          filter(row_number() == 1L) %>% 
          ungroup()
        pnt <-
          pnt %>%
          mutate(dummy = 1L) %>% 
          left_join(color %>% mutate(dummy = 1L), by = "dummy") %>% 
          filter(spp <= value) %>% 
          select(-dummy) %>% 
          group_by(name, lng, lat) %>% 
          arrange(value, .by_group = TRUE) %>% 
          filter(row_number() == 1L) %>% 
          ungroup() %>% 
          select(lng, lat, value, name)
      }
    
    pnt_rtm <-
      spp_rtm %>% 
      add_lng_lat_cols()
    pnt_rtm
    pnt_dam <-
      spp_dam %>% 
      add_lng_lat_cols()
    pnt_dam
    pnt_rtm %>% teproj::export_path(config$path_pnt_rtm)
    pnt_dam %>% teproj::export_path(config$path_pnt_dam)
  } else {
    stop(call. = FALSE)
  }
}

# spdf_base_map ----
cnd <- file.exists(config$path_spdf_base_map)
if(cnd) {
  spdf_base_map <-
    config$path_spdf_base_map %>% 
    read_rds()
} else {
  z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
  spdf_base_map <- readRDS(z)
  spdf_base_map <- sp::spTransform(spdf_base_map, sp::CRS("+proj=longlat +datum=WGS84"))
  # spdf_base_map %>% teproj::export_path(config$path_spdf_base_map)
  spdf_base_map %>% write_rds(config$path_spdf_base_map)
}

# spdf_pnt_tess_trim ----
cnd <-
  (
    file.exists(config$path_sf_pnt_rtm) & 
      file.exists(config$path_sf_pnt_dam) & 
      file.exists(config$path_sf_poly_rtm) &
      file.exists(config$path_sf_poly_dam)
  )
if(cnd) {
  sf_pnt_rtm <-
    config$path_sf_pnt_rtm %>% 
    teproj::import_path_cleanly()
  sf_poly_rtm <-
    config$path_sf_poly_rtm %>% 
    teproj::import_path_cleanly()
  sf_pnt_dam <-
    config$path_sf_pnt_dam %>% 
    teproj::import_path_cleanly()
  sf_poly_dam <-
    config$path_sf_poly_dam %>% 
    teproj::import_path_cleanly()
} else {
  create_spdf <-
    function(pnt) {
      spdf_pnt <-
        sp::SpatialPointsDataFrame(
          cbind(pnt$lng, pnt$lat),
          pnt,
          match.ID = TRUE
        )
      spdf_pnt@bbox <- spdf_base_map@bbox
      sp::proj4string(spdf_pnt) <- sp::proj4string(spdf_base_map)
      spdf_pnt
    }
  
  spdf_pnt_rtm <-
    pnt_rtm %>%
    create_spdf()
  spdf_pnt_dam <-
    pnt_dam %>%
    create_spdf()
  
  sf_pnt_rtm <-
    spdf_pnt_rtm %>%
    sf::st_as_sf()
  sf_pnt_dam <-
    spdf_pnt_dam %>%
    sf::st_as_sf()
  
  sf_pnt_rtm %>% readr::write_rds(config$path_sf_pnt_rtm)
  sf_pnt_dam %>% readr::write_rds(config$path_sf_pnt_dam)
  
  convert_spdf_pnt_to_sf_poly <-
    function(spdf_pnt) {
      th <-
        spdf_pnt %>% 
        maptools::as.ppp.SpatialPointsDataFrame() %>% 
        spatstat::dirichlet() %>% 
        methods::as("SpatialPolygons")
      sp::proj4string(th) <- sp::proj4string(spdf_pnt)
      th_z <- sp::over(th, spdf_pnt)
      spdf_pnt_poly <- sp::SpatialPolygonsDataFrame(th, th_z)
      spdf_pnt_poly_trim <- raster::intersect(spdf_base_map, spdf_pnt_poly)
      sf_poly_trim <- spdf_pnt_poly_trim %>% sf::st_as_sf()
    }

  sf_poly_rtm <-
    spdf_pnt_rtm %>%
    convert_spdf_pnt_to_sf_poly()
  sf_poly_dam <-
    spdf_pnt_dam %>%
    convert_spdf_pnt_to_sf_poly()
  
  sf_poly_rtm %>% readr::write_rds(config$path_sf_poly_rtm)
  sf_poly_dam %>% readr::write_rds(config$path_sf_poly_dam)
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

visualize_pnt <-
  function(sf_poly, sf_pnt, ...) {
    viz_pnt <-
      ggplot() +
      geom_sf(
        data =
          sf_poly %>% 
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
        caption = "By Tony ElHabr."
      )
  }
viz_pnt_rtm <-
  visualize_pnt(
    sf_poly = sf_poly_rtm,
    sf_pnt = sf_pnt_rtm
  ) +
  labs(
    title = "ERCOT, Real-Time LMP Prices"
  )
viz_pnt_rtm

viz_pnt_dam <-
  visualize_pnt(
    sf_poly = sf_poly_dam,
    sf_pnt = sf_pnt_dam
  ) +
  labs(
    title = "ERCOT, Day-Ahead LMP Prices"
  )
viz_pnt_dam

.UNITS = "in"
.HEIGHT = 8L
.WIDTH = 8L
save_viz_pnt <-
  function(x,
           file = deparse(substitute(x)),
           export = config$export_viz,
           dir = config$dir_viz,
           units = .UNITS,
           height = .HEIGHT,
           width = .WIDTH,
           ...) {
    teproj::export_ext_png(
      x = x,
      file = file,
      export = export,
      dir = dir,
      units = units,
      height = height,
      width = width
    )
  }

save_viz_pnt(viz_pnt_rtm)
save_viz_pnt(viz_pnt_dam)
