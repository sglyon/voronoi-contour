

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
  purrr::set_names(c("name", "value")) %>% 
  mutate_at(vars(value), funs(as.numeric))
lmp_pnt

lmp_pnt %>% teproj::export_path(config$path_lmp)
kml_pnt <-
  config$path_kml_pnt %>% 
  teproj::import_path_cleanly()
pnt0 <-
  kml_pnt %>%
  inner_join(lmp_pnt)
pnt0

pnt0 %>%
  count(lng, lat, sort = TRUE)

pnt <-
  pnt0 %>%
  group_by(lng, lat) %>%
  summarise_at(vars(value), funs(max)) %>%
  ungroup() %>% 
  inner_join(pnt0) %>% 
  group_by(lng, lat) %>% 
  filter(row_number() == 1L) %>% 
  ungroup()
pnt %>% teproj::export_path(config$path_pnt)
# library("teplot")
# viz_kml <-
#   teplot::create_map_base_tx() +
#   geom_point(
#     data = summ_pnt,
#     aes(x = lng, y = lat, group = "")
#   ) +
#   ggvoronoi::geom_voronoi(
#     data = summ_pnt,
#     aes(x = lng, y = lat, fill = value, group = "")
#   ) +
#   teplot::theme_map()
# viz_kml
