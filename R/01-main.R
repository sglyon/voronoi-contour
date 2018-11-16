
cnd <-
  !(
      file.exists(config$path_png_map_spp_rtm_raw) &
      file.exists(config$path_png_map_spp_dam_raw) &
        file.exists(config$path_png_legend_raw)
  )

if(cnd) {
  download_png <- function(url = NULL, path, mkt = c("rtm", "dam"), hour = NULL) {
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
cnd <- 
  file.exists(config$path_kml_pnt_rtm) & file.exists(config$path_kml_pnt_dam)
if(cnd) {
  kml_pnt_rtm <-
    config$path_kml_pnt_rtm %>% 
    teproj::import_path_cleanly()
  kml_pnt_dam <-
    config$path_kml_pnt_dam %>% 
    teproj::import_path_cleanly()
} else {
  
  convert_kml_raw_to_kml_pnt <-
    function(path) {
      kml_pnt_raw <-
        path %>%
        tidykml::kml_points()
      kml_pnt <-
        kml_pnt_raw %>%
        select(name, lat = latitude, lng = longitude) %>%
        distinct(name, lat, lng) %>%
        arrange(name)
    }
  kml_pnt_rtm <-
    config$path_kml_rtm_raw %>%
    convert_kml_raw_to_kml_pnt()
  kml_pnt_rtm
  kml_pnt_dam <-
    config$path_kml_dam_raw %>%
    convert_kml_raw_to_kml_pnt()
  kml_pnt_dam
  kml_pnt_rtm %>% anti_join(kml_pnt_dam)
  teproj::export_path(kml_pnt_rtm, config$path_kml_pnt_rtm)
  teproj::export_path(kml_pnt_dam, config$path_kml_pnt_dam)
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
  teproj::export_path(color, config$path_color)
}

# spp_pnt ----
cnd <- 
  (
    file.exists(config$path_pnt_rtm) & 
      file.exists(config$path_pnt_dam) & 
      file.exists(config$path_pnt_rtm2dam)
    )
if(cnd) {
  pnt_rtm <-
    config$path_pnt_rtm %>% 
    teproj::import_path_cleanly()
  pnt_dam <-
    config$path_pnt_dam %>% 
    teproj::import_path_cleanly()
  pnt_rtm2dam <-
    config$path_pnt_rtm2dam %>% 
    teproj::import_path_cleanly()
} else {
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
  
  datetime_filt <- lubridate::ymd_hms("2018-11-15 08:00:00")
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
  
  # Debugging...
  kml_pnt_rtm %>% anti_join(spp_dam)
  kml_pnt_dam %>% anti_join(spp_rtm)
  
  spp_rtm2dam <-
    inner_join(
      spp_rtm %>% select(-datetime) %>% rename(spp_rtm = spp),
      spp_dam %>% select(-datetime) %>% rename(spp_dam = spp)
    ) %>% 
    mutate(spp = spp_rtm - spp_dam)
  
  add_lng_lat_cols <-
    function(spp, kml_pnt) {
      pnt <-
        kml_pnt %>%
        inner_join(spp)
      
      pnt <-
        pnt %>%
        group_by(lng, lat) %>%
        summarise_at(vars(spp), funs(max)) %>%
        ungroup() %>% 
        inner_join(pnt) %>% 
        group_by(lng, lat) %>% 
        filter(row_number() == 1L) %>% 
        ungroup()
    }
  
  join_colors <-
    function(pnt) {
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
    add_lng_lat_cols(kml_pnt_rtm) %>% 
    join_colors()
  pnt_rtm
  pnt_dam <-
    spp_dam %>% 
    add_lng_lat_cols(kml_pnt_dam) %>% 
    join_colors()
  pnt_dam
  
  pnt_rtm2dam <-
    spp_rtm2dam %>%
    add_lng_lat_cols(kml_pnt_rtm)
  pnt_rtm2dam
  
  teproj::export_path(pnt_rtm, config$path_pnt_rtm)
  teproj::export_path(pnt_dam, config$path_pnt_dam)
  teproj::export_path(pnt_rtm2dam, config$path_pnt_rtm2dam)
}

# # spdf_base_map ----
# cnd <- file.exists(config$path_spdf_base_map)
# if(cnd) {
#   spdf_base_map <-
#     config$path_spdf_base_map %>% 
#     read_rds()
# } else {
#   z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
#   spdf_base_map <- readRDS(z)
#   spdf_base_map <- sp::spTransform(spdf_base_map, sp::CRS("+proj=longlat +datum=WGS84"))
#   # spdf_base_map %>% teproj::export_path(config$path_spdf_base_map)
#   spdf_base_map %>% write_rds(config$path_spdf_base_map)
# }

# spdf_bound ----
cnd <- file.exists(config$path_spdf_bound)
if(cnd) {
  spdf_bound <-
    config$path_spdf_bound %>% 
    read_rds()
} else {
  bound <-
    config$path_kml_bound %>% 
    xml2::read_xml() %>%
    xml2::xml_children() %>%
    xml2::xml_children() %>%
    magrittr::extract(4) %>%
    xml2::xml_children() %>%
    magrittr::extract(3) %>%
    xml2::xml_children() %>%
    magrittr::extract(4) %>%
    xml2::xml_children() %>%
    magrittr::extract(4) %>%
    xml2::xml_text() %>% 
    # readLines()
    str_split(",0\\s+", simplify = TRUE) %>% 
    str_replace_all("[^(0-9|.|,|\\-)]","")  %>% 
    str_split(",", simplify = TRUE) %>% 
    as_tibble() %>% 
    purrr::set_names(c("lng", "lat")) %>% 
    mutate_all(funs(as.numeric)) %>% 
    filter(!is.na(lng) & !is.na(lat)) %>% 
    bind_rows(
      .,
      slice(., 1L)
    )
  bound_nontop <-
    bound %>%
    arrange(desc(row_number())) %>% 
    slice(c(24:n()))

  bound_state <-
    teplot::get_map_data_state(state = "tx") %>%
    as_tibble()
  bound_top <-
    bound_state %>%
    slice(c(840:870)) %>% 
    select(lng = long, lat)
    
  bound_fix <-
    bind_rows(
      bound_top,
      bound_nontop
    ) %>% 
    bind_rows(
      .,
      slice(., 1L)
    )
  # bound_fix %>% count(lng, lat)
  bound_fix %>%
    ggplot(aes(lng, lat)) +
    geom_path()
  bound_poly <-
    bound_fix %>% 
    sp::Polygon() %>% 
    list() %>% 
    sp::Polygons(ID = "dummy") %>% 
    list() %>%  
    sp::SpatialPolygons()
  spdf_bound <-
    sp::SpatialPolygonsDataFrame(
      bound_poly, 
      data.frame(state = c("texas"), row.names = c("dummy"))
    )
  sp::proj4string(spdf_bound) <- sp::CRS("+init=epsg:4326")
  # # NOTE: This is actually the same coordinate system. By transforming here,
  # # removing some of the attributes to match with `spdf_pnt`.
  # spdf_bound <- sp::spTransform(spdf_bound, sp::CRS("+proj=longlat +datum=WGS84"))
  # spdf_bound
  readr::write_rds(spdf_bound, config$path_spdf_bound)
  
}

# sf_poly ----
cnd <-
  (
    file.exists(config$path_sf_pnt_rtm) & 
      file.exists(config$path_sf_pnt_dam) & 
      file.exists(config$path_sf_pnt_rtm2dam) & 
      file.exists(config$path_sf_poly_rtm) &
      file.exists(config$path_sf_poly_dam) &
      file.exists(config$path_sf_poly_rtm2dam)
  )
if(cnd) {
  sf_pnt_rtm <-
    config$path_sf_pnt_rtm %>% 
    teproj::import_path_cleanly()
  sf_pnt_dam <-
    config$path_sf_pnt_dam %>% 
    teproj::import_path_cleanly()
  sf_pnt_rtm2dam <-
    config$path_sf_pnt_rtm2dam %>% 
    teproj::import_path_cleanly()
  
  sf_poly_rtm <-
    config$path_sf_poly_rtm %>% 
    teproj::import_path_cleanly()
  sf_poly_dam <-
    config$path_sf_poly_dam %>% 
    teproj::import_path_cleanly()
  sf_poly_rtm2dam <-
    config$path_sf_poly_rtm2dam %>% 
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
      # spdf_pnt@bbox <- spdf_base_map@bbox
      # sp::proj4string(spdf_pnt) <- sp::proj4string(spdf_base_map)
      spdf_pnt@bbox <- spdf_bound@bbox
      sp::proj4string(spdf_pnt) <- sp::proj4string(spdf_bound)
      spdf_pnt
    }
  
  spdf_pnt_rtm <-
    pnt_rtm %>%
    create_spdf()
  spdf_pnt_dam <-
    pnt_dam %>%
    create_spdf()
  spdf_pnt_rtm2dam <-
    pnt_rtm2dam %>%
    create_spdf()
  
  sf_pnt_rtm <-
    spdf_pnt_rtm %>%
    sf::st_as_sf()
  sf_pnt_dam <-
    spdf_pnt_dam %>%
    sf::st_as_sf()
  sf_pnt_rtm2dam <-
    spdf_pnt_rtm2dam %>%
    sf::st_as_sf()
  
  readr::write_rds(sf_pnt_rtm, config$path_sf_pnt_rtm)
  readr::write_rds(sf_pnt_dam, config$path_sf_pnt_dam)
  readr::write_rds(sf_pnt_rtm2dam, config$path_sf_pnt_rtm2dam)
  
  convert_spdf_pnt_to_sf_poly <-
    function(spdf_pnt) {
      # spdf_pnt <- spdf_pnt_rtm
      th <-
        spdf_pnt %>% 
        maptools::as.ppp.SpatialPointsDataFrame() %>% 
        spatstat::dirichlet() %>% 
        methods::as("SpatialPolygons")
      sp::proj4string(th) <- sp::proj4string(spdf_pnt)
      th_z <- sp::over(th, spdf_pnt)
      spdf_pnt_poly <- sp::SpatialPolygonsDataFrame(th, th_z)
      # spdf_pnt_poly_trim <- raster::intersect(spdf_base_map, spdf_pnt_poly)
      spdf_pnt_poly_trim <- raster::intersect(spdf_bound, spdf_pnt_poly)
      sf_poly_trim <- spdf_pnt_poly_trim %>% sf::st_as_sf()
    }
  
  sf_poly_rtm <-
    spdf_pnt_rtm %>%
    convert_spdf_pnt_to_sf_poly()
  sf_poly_dam <-
    spdf_pnt_dam %>%
    convert_spdf_pnt_to_sf_poly()
  sf_poly_rtm2dam <-
    spdf_pnt_rtm2dam %>%
    convert_spdf_pnt_to_sf_poly()
  
  readr::write_rds(sf_poly_rtm, config$path_sf_poly_rtm)
  readr::write_rds(sf_poly_dam, config$path_sf_poly_dam)
  readr::write_rds(sf_poly_rtm2dam, config$path_sf_poly_rtm2dam)
}

# viz_pnt ----
bound_state <-
  teplot::get_map_data_state(state = "tx") %>%
  as_tibble()
bound_county <-
  teplot::get_map_data_county(state = "tx") %>% 
  as_tibble()

add_bound_layers <-
  function(viz) {
    viz +
      geom_polygon(
        data = bound_county,
        aes(x = long, y = lat, group = group),
        size = 0.1,
        color = "black",
        fill = NA
      ) +
      geom_polygon(
        data = bound_state,
        aes(x = long, y = lat, group = group),
        size = 1,
        color = "black",
        fill = NA
      )
  }

labs_fill <-
  color$value %>%
  scales::dollar() %>% 
  paste0("<" , ., ".00")

visualize_pnt <-
  function(sf_poly, sf_pnt) {
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
      # add_bound_layers() +
      geom_polygon(
        data = bound_county,
        aes(x = long, y = lat, group = group),
        size = 0.1,
        color = "black",
        fill = NA
      ) +
      geom_polygon(
        data = bound_state,
        aes(x = long, y = lat, group = group),
        size = 1,
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
        legend.key.height = unit(0.55, "mm"),
        # legend.position = c(0.975, 0.1),
        legend.position = "right",
        panel.background = element_blank()
      )
  }

viz_pnt_rtm <-
  visualize_pnt(
    sf_poly = sf_poly_rtm,
    sf_pnt = sf_pnt_rtm
  )
viz_pnt_rtm

lab_subtitle <- "11/15/2018 7:15 AM"
lab_caption <- "By Tony ElHabr."
viz_pnt_rtm_banner <-
  viz_pnt_rtm +
  labs(
    title = "Real-Time ERCOT Settlment Point Prices",
    subtitle = lab_subtitle,
    caption = lab_caption
  )

viz_pnt_dam <-
  visualize_pnt(
    sf_poly = sf_poly_dam,
    sf_pnt = sf_pnt_dam
  )
viz_pnt_dam

viz_pnt_rtm2dam <-
  ggplot() +
  geom_sf(
    data =
      sf_poly_rtm2dam,
    aes(fill = value),
    color = NA
  ) +
  scale_fill_viridis_c(option = "E") +
  guides(
    fill = guide_legend(reverse = TRUE)
  ) +
  # add_bound_layers() +
  geom_polygon(
    data = bound_county,
    aes(x = long, y = lat, group = group),
    size = 0.1,
    color = "black",
    fill = NA
  ) +
  geom_polygon(
    data = bound_state,
    aes(x = long, y = lat, group = group),
    size = 1,
    color = "black",
    fill = NA
  ) +
  geom_sf(
    data = sf_pnt_rtm2dam,
    shape = 21,
    size = 2,
    stroke = 1,
    color = "red",
    fill = "black"
  ) +
  coord_sf(datum = NA) +
  teplot::theme_map() +
  theme(
    # legend.key.height = unit(0.55, "mm"),
    # legend.position = c(0.975, 0.1),
    legend.position = "right",
    panel.background = element_blank()
  ) +
  labs(
    title = "(Real-Time - Day-Ahead) ERCOT Settlement Point Prices",
    subtitle = lab_subtitle,
    caption = lab_caption
  )
viz_pnt_rtm2dam

# viz_pnt_dam +
#   theme(
#     legend.key.height = unit(0.55, "mm"),
#     legend.position = c(0.975, 0.05)
#   )

export_viz_pnt <-
  function(x,
           file = deparse(substitute(x)),
           export = config$export_viz,
           dir = config$dir_viz,
           units = "in",
           height = 8L,
           width = 9L,
           ...) {
    teproj::export_ext_png(
      x = x,
      file = file,
      export = export,
      dir = dir,
      units = units,
      height = height,
      width = width, 
      ...
    )
  }

export_viz_pnt(viz_pnt_rtm_banner)
export_viz_pnt(viz_pnt_rtm2dam)
path_viz_pnt_rtm <- export_viz_pnt(viz_pnt_rtm)
path_viz_pnt_dam <- export_viz_pnt(viz_pnt_dam)

read_info <- function(x) {
  x %>%
    magick::image_read() %>%
    magick::image_info()
}

read_append <- function(x) {
  x %>%
    magick::image_read() %>%
    magick::image_append()
}

img_info <-
  config$path_png_map_spp_rtm_raw %>%
  read_info()
img_info_legend <-
  config$path_png_legend_raw %>%
  read_info()
h <- img_info %>% pluck("height")
w <- img_info %>% pluck("width")
h_legend <- img_info_legend %>% pluck("height")
w_legend <- img_info_legend %>% pluck("width")

h_diff <- h - h_legend
bkgrd <-
  magick::image_blank(
    width = 1 * w + w_legend,
    height = h,
    col = "#FFFFFF"
  )

export_png_map <-
  function(path_in, path_out = file.path(config$dir_viz, basename(path_in))) {
    bkgrd %>%
      magick::image_composite(
        path_in %>%
          read_append(),
        offset = paste0("+", 0 * w, "+", 0 * h)
      ) %>%
      magick::image_composite(
        config$path_png_legend_raw %>%
          read_append(),
        offset = paste0("+", 1 * w, "+", 0 + h_diff)
      ) %>%
      magick::image_write(path_out)
    invisible(path_out)
  }

path_png_map_spp_rtm <- export_png_map(config$path_png_map_spp_rtm_raw)
path_png_map_spp_dam <- export_png_map(config$path_png_map_spp_dam_raw)

img_info1 <- path_viz_pnt_rtm %>% read_info()
h1 <- img_info1 %>% pluck("height")
w1 <- img_info1 %>% pluck("width")

# img_info2 <- path_png_map_spp_rtm %>% read_info()
# h2 <- img_info2 %>% pluck("height")
# w2 <- img_info2 %>% pluck("width")
# ratio_h <- h1 / h2
# ratio_w <- w1 / w2
# h2_resized <- h2 * ratio_h
# w2_resized <- w2 * ratio_w

bkgrd12 <-
  magick::image_blank(
    width = 1 * w1 + 0.85 * w1,
    height = h1,
    col = "#FFFFFF"
  )

export_png_map_compare <-
  function(path_in1,
           path_in2,
           path_out =
             file.path(config$dir_viz,
                       paste0(tools::file_path_sans_ext(basename(path_in1)), "-compare.png"))) {
    
    img2_resized <-
      path_in2 %>%
      magick::image_read() %>%
      # magick::image_resize(paste0(w2_resized, "x", h2_resized))
      magick::image_resize(paste0(0.85 * w1, "x", 0.85 * h1))
    bkgrd12 %>%
      magick::image_composite(
        path_in1 %>%
          read_append(),
        offset = paste0("+", 0 * w1)
      ) %>%
      magick::image_composite(
        img2_resized %>%
          magick::image_append(),
        offset = paste0("+", 1 * w1, "+", 1 * 300)
      ) %>%
      magick::image_write(path_out)
    invisible(path_out) 
  }

path_viz_pnt_rtm_compare <-
  export_png_map_compare(
    path_in1 = path_viz_pnt_rtm,
    path_in2 = path_png_map_spp_rtm
  )

path_viz_pnt_dam_compare <-
  export_png_map_compare(
    path_in1 = path_viz_pnt_dam,
    path_in2 = path_png_map_spp_dam
  )

