
path_kml_boundary <-
  file.path("data-raw", "ercot_boundary.kml")
kml_raw <-
  path_kml_boundary %>%
  tidykml::kml_read()
bound <-
  path_kml_boundary %>% 
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
  filter(!is.na(lng) & !is.na(lat))
bound %>%
  ggplot(aes(x = lng, y = lat)) +
  geom_path()
# method x1 ----
# # Reference: https://gist.github.com/holstius/6631918.
# read_kml <- function(file, layers) {
#   require(sp)
#   require(rgdal)
#   read.layer <- function (layer_name) {
#     spobj <- rgdal::readOGR(dsn=file, layer=layer_name)
#     coords <- coordinates(spobj)
#     colnames(coords) <- c('x', 'y', 'z')[1:ncol(coords)]
#     df <- data.frame(coords, spobj@data)
#     transform(df, layer=layer_name)
#   }
#   Reduce(rbind, lapply(layers, read.layer))
# }
# kml_read <-
#   config$path_kml_raw %>% 
#   read_kml()

# method x2 ----
# rg <-
#   config$path_kml_raw %>% 
#   rgdal::readOGR()
# rg$Name
# rg$Description

# good ----
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

# extra ----
kml_poly_raw <-
  config$path_kml_raw %>%
  tidykml::kml_polygons()
kml_poly_raw

# kml_lines <-
#   function(x, ns = "d1", verbose = TRUE, ...) {
#     # x <- tidykml::kml_read(x, ...)
#     x <- config$path_kml_raw
#     x <- tidykml::kml_read(x)
#     ns <- "d1"
#     y <- tidykml:::kml_folders(x, ns)
#     
#     if (!length(y)) {
#       # case: no folders
#       x <- xml2::xml_find_all(x, str_c("//", ns, ":Document"))
#       
#     } else {
#       x <- y
#       y <- length(y)
#       
#     }
#     xtemp <- x
#     # browser()
#     xout <- lapply(x, function(x) {
#       f <- tidykml:::kml_element(x, "name", ns)
#       # xi <- tidykml:::kml_placemarks(x, "LineString", ns)
#       xi <- tidykml:::kml_placemarks(x, "Contour Levels", ns)
#       
#       # case: no placemarks
#       if (!length(xi)) {
#         return(NULL)
#       }
#       
#       data_frame(
#         folder = f,
#         name = tidykml:::kml_element(xi, "name", ns),
#         description = tidykml:::kml_element(xi, "description", ns),
#         styleUrl = tidykml:::kml_element(xi, "styleUrl", ns),
#         coordinates = tidykml:::kml_element(xi, str_c("LineString/", ns, ":coordinates"), ns)
#       )
#       
#     }) %>%
#       bind_rows
#     #
#     #   coords <-
#     #     lapply(xtemp, function(x) {
#     #
#     #       f <- tidykml:::kml_element(x, "name", ns)
#     #       x <- tidykml:::kml_placemarks(x, "LineString", ns)
#     #
#     #       # case: no placemarks
#     #       if (!length(x)) {
#     #         return(NULL)
#     #       }
#     #
#     #       data_frame(
#     #         coordinates = tidykml:::kml_element(x, str_c(
#     #           "LineString/",
#     #           ns,
#     #           ":coordinates"),
#     #           ns) %>%
#     #           str_split("\\s+") %>%
#     #           unlist
#     #       )
#     #
#     #     }) %>%
#     #     bind_rows
#     
#     # return(tidykml:::kml_finalize(x, folders = length(y) > 0, verbose))
#     # list(x = x, coords = coords)
#     xout
#     
#   }

kml_line_raw <-
  config$path_kml_raw %>%
  # tidykml::kml_lines()
  kml_lines()
kml_line_raw

kml_bound_raw <-
  config$path_kml_raw %>%
  tidykml::kml_bounds()
kml_bound_raw

kml_info <-
  config$path_kml_raw %>%
  tidykml::kml_info()
kml_info

kml_raw <-
  config$path_kml_raw %>%
  tidykml::kml_read()
xml_pnt <-
  config$path_kml_raw %>%
  xml2::read_xml() %>%
  xml2::xml_children() %>%
  xml2::xml_children() %>%
  magrittr::extract(40) %>%
  xml2::xml_children()

xml_legend <-
  config$path_kml_raw %>%
  xml2::read_xml() %>%
  xml2::xml_children() %>%
  xml2::xml_children() %>%
  magrittr::extract(3:36)

legend_id <-
  xml_legend %>% 
  xml2::xml_attr("id")

legend_colors <-
  xml_legend %>% 
  xml2::xml_text() %>% 
  str_replace_all("^0CA", "") %>% 
  str_replace_all("([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})", "\\3\\2\\1") %>% 
  paste0("#", .)
legend_colors

# colors_rgb <-
#   legend_colors %>% 
#   grDevices::col2rgb() %>%
#   t() %>%
#   as.data.frame() %>%
#   as_tibble()
# colors_rgb

values_colors <-
  c(-250L,
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
    9000L)
