
# bound_aug %>% 
#   maptools::as.ppp.SpatialPointsDataFrame() %>%
#   spatstat::dirichlet() %>%
#   methods::as("SpatialPolygons")
# 
#   sp::Polygon()
#   
#   as.matrix() %>%
#   list() %>%
#   sf::st_polygon()

# https://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html
x1 = c(1, 1); y1 = c(2, 2)
c1 = cbind(x1, y1)
r1 = rbind(c1, c1[1, ])
P1 = Polygon(r1)
Ps1 = Polygons(list(P1), ID = "a")
SPs = SpatialPolygons(list(Ps1))
SPDF = SpatialPolygonsDataFrame(SPs, data.frame(N = c("one"), row.names = c("a")))
SPDF
spdf_base_map
proj4string(SPDF)
latlong = "+init=epsg:4326"
proj4string(SPDF) = CRS(latlong)
SPDF
sp::proj4string(SPDF) <- sp::proj4string(spdf_base_map)
SPDF
