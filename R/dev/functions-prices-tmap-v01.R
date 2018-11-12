



library("dplyr")

# https://mgimond.github.io/Spatial/interpolation-in-r.html
library("rgdal")
library("tmap")
library("spatstat")
library("maptools")
library("raster")
library("gstat")
library("sp")
library("deldir")

library("animation")


# How to re-create base_map.rds.
# z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
# W <- readRDS(z)
# W2 <- spTransform(W, CRS("+proj=longlat +datum=WGS84"))
# saveRDS(W2, "data/external/base_map.rds")

import_base_map_spdf <- function(filepath = filepath_base_map) {
  if (is.null(filepath)) {
    stop("Missing filepath.")
  }
  readRDS(filepath)
}

get_tm_layout_base <- function(main_title = NA) {
  tm_layout(
    title = main_title,
    # title.size = 1.3,
    outer.bg.color = "black",
    # frame = FALSE,
    legend.outside = TRUE,
    legend.frame = TRUE,
    #  panel.show = FALSE,
    panel.label.bg.color = "white"
  )
}

get_tm_layout_pal <- function() {
  tm_layout(
    legend.format = list(
      text.to.columns = TRUE,
      text.separator = "-",
      fun = function(x)
        sprintf(" $ %.0f", x)
    )
  )
}

# spdf_base_map <- import_base_map_spdf()

get_tm_base_map <- function(spdf_base_map) {
  if (missing(spdf_base_map)) {
    spdf_base_map <- import_base_map_spdf()
  }
  tm <-
    tm_shape(spdf_base_map) +
    tm_polygons(col = "white", alpha = 0)
  tm
}

get_tm_base_map_dots <-
  function(spdf_base_map, spdf, points, point_size) {
    tm <- get_tm_base_map(spdf_base_map = spdf_base_map)
    if (points) {
      tm <- tm + tm_shape(spdf) + tm_dots(size = point_size)
    }
    tm
  }

adjust_pal_breaks <- function(pal_breaks, pal_style) {
  if (!(pal_style %in% c("fixed", "cont"))) {
    pal_breaks <- NULL
  }
  pal_breaks
}

# Modified from https://rud.is/b/2015/07/26/making-staticinteractive-voronoi-map-layers-in-ggplotleaflet/.
convert_spointsdf_to_spolysdf_v2 <- function(sp) {
  # tile.list extracts the polygon data from the deldir computation
  vor_desc <- deldir::tile.list(deldir::deldir(sp@coords[, 1], sp@coords[, 2]))

  lapply(1:(length(vor_desc)), function(i) {
    # tile.list gets us the points for the polygons but we
    # still have to close them, hence the need for the rbind
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1, ])

    # now we can make the Polygon(s)
    Polygons(list(Polygon(tmp)), ID = i)
  }) -> vor_polygons

  # hopefully the caller passed in good metadata!
  sp_dat <- sp@data

  # this way the IDs _should_ match up w/the data & voronoi polys
  rownames(sp_dat) <- sapply(
    slot(
      SpatialPolygons(vor_polygons),
      "polygons"
    ),
    slot, "ID"
  )
  out <-
    SpatialPolygonsDataFrame(
      SpatialPolygons(vor_polygons),
      data = sp_dat
    )
  out
}

# Modified from https://mgimond.github.io/Spatial/interpolation-in-r.html.
convert_price_data_to_spdf <- function(data, spdf_base_map) {
  if (missing(spdf_base_map)) {
    spdf_base_map <- import_base_map_spdf()
  }
  spdf <-
    SpatialPointsDataFrame(
      cbind(data$lon, data$lat),
      data,
      match.ID = TRUE
    )
  spdf@bbox <- spdf_base_map@bbox
  proj4string(spdf) <- proj4string(spdf_base_map)
  spdf
}

# Modified from https://mgimond.github.io/Spatial/interpolation-in-r.html.
convert_spointsdf_to_spolysdf <- function(spdf) {
  th <-
    methods::as(spatstat::dirichlet(spatstat::as.ppp(spdf)), "SpatialPolygons")
  sp::proj4string(th) <- sp::proj4string(spdf)
  th_z <- sp::over(th, spdf, fn = mean)
  spdf_tess <- sp::SpatialPolygonsDataFrame(th, th_z)
  spdf_tess
}

# Modified from https://mgimond.github.io/Spatial/interpolation-in-r.html.
create_spdf_tess_trimmed <-
  function(spdf, spdf_base_map) {
    if (missing(spdf_base_map)) {
      spdf_base_map <- import_base_map_spdf()
    }

    spdf_tess <- convert_spointsdf_to_spolysdf(spdf)
    # spdf_tess <- convert_spointsdf_to_spolysdf_v2(spdf)
    spdf_tess_trimmed <- raster::intersect(spdf_base_map, spdf_tess)
    spdf_tess_trimmed
  }

# NOTE: `breaks` is ingored unless `style == "fixed"` or `style == "cont"`.
# NOTE: `pal_n` is not relevant if `pal_style == "fixed"`.
visualize_price_tess <-
  function(data,
           col_value = "price",
           spdf,
           spdf_base_map,
           spdf_tess_trimmed,
           main_title = NA,
           legend = TRUE,
           legend_title = NA,
           pal_n = 10,
           pal_style = "fixed",
           pal_breaks = pal_fixed_labs,
           pal_hex = pal_fixed_hex,
           hist = TRUE,
           points = TRUE,
           point_size = 0.1,
           ...) {
    if (missing(data) & missing(spdf)) {
      stop("Missing required input.")
    } else if (!missing(data) & missing(spdf)) {
      spdf <- convert_price_data_to_spdf(data)
    } else if (missing(data) & !missing(spdf)) {

    }

    if (missing(spdf_base_map)) {
      spdf_base_map <- import_base_map_spdf()
    }

    if (missing(spdf_tess_trimmed) & !missing(spdf)) {
      spdf_tess_trimmed <- create_spdf_tess_trimmed(spdf, spdf_base_map)
    }

    tm_layout_base <- get_tm_layout_base(main_title)
    tm_layout_pal <- get_tm_layout_pal()
    tm_base_map_dots <-
      get_tm_base_map_dots(spdf_base_map, spdf, points, point_size)
    pal_breaks <- adjust_pal_breaks(pal_breaks, pal_style)

    tm <-
      tm_shape(spdf_tess_trimmed) +
      tm_polygons(
        title = legend_title,
        alpha = 1,
        col = col_value,
        border.col = "black",
        border.alpha = 0,
        n = pal_n,
        palette = pal_hex,
        style = pal_style,
        breaks = pal_breaks,
        legend.show = legend,
        legend.reverse = TRUE,
        legend.hist = hist,
        auto.palette.mapping = FALSE
      ) +
      tm_base_map_dots +
      tm_layout_pal +
      tm_layout_base

    tm
  }

# NOTE: `filepath` isn't currently used because a workaround is used instead.
visualize_price_tess_byint <-
  function(data,
           col_int = "hour",
           col_int_other = c("year", "month", "day"),
           # yyyymmdd = paste0(strftime(Sys.Date(), "%Y-%m-%d"), "_today"),
           ints,
           spdf_base_map,
           animate = TRUE,
           dir_png = dir_png,
           filename_prefix_png = paste0(col_int, "_"),
           ext_png = "png",
           save_png = TRUE,
           filepath_gif = file.path(dir_png, paste0(filename_prefix_png, "all", ".", "gif")),
           interval = 0.5,
           ...) {
    if (missing(ints)) {
      ints <- get_distinct_time_int(data, col_int)
    }
    # ints_other <- get_distinct_time_int(data, col_int_other)
    # if(length(ints_other) > 1) {
    #   warning("Aggregating over more than one other time interval.")
    # }
    if (missing(spdf_base_map)) {
      spdf_base_map <- import_base_map_spdf()
    }
    tms <- vector("list", length(ints))
    filepaths <- vector("list", length(ints))
    i <- 1
    while (i <= length(ints)) {
      int_i <- ints[i]
      data_i <- data %>% filter(!!rlang::sym(col_int) == int_i)
      data_processed_i <- process_price_data(data_i)
      spdf_i <- convert_price_data_to_spdf(data_processed_i)
      spdf_tess_i_trimmed <- create_spdf_tess_trimmed(spdf_i, spdf_base_map)

      tm_i <-
        visualize_price_tess(
          spdf = spdf_i,
          spdf_base_map = spdf_base_map,
          spdf_tess_trimmed = spdf_tess_i_trimmed,
          main_title = paste0(stringr::str_to_title(col_int), " ", int_i),
          legend = FALSE
        )

      tms[i] <- list(tm_i)
      names(tms[i]) <- i
      filepath_i <-
        file.path(dir_png,
                  paste0(
                    filename_prefix_png,
                    sprintf("%02.f", int_i), ".", ext_png
                  )
        )
      filepaths[i] <- list(filepath_i)
      names(filepaths[i]) <- i
      if (save_png) {

        # browser()
        save_tmap(
          verbose = TRUE,
          tm = tm_i,
          filename = filepath_i,
          # width = 1920,
          width = 1200,
          # height = 1080,
          height = 1000,
          asp = 0,
          units = "px",
          dpi = 300
        )
      }
      message("Done processing interval ", int_i, ".")
      i <- i + 1
    }

    # message("Done processing intervals.\n")
    if (animate) {
      convert_tms_to_gif(tms = tms, ints = ints, filepath_gif = filepath_gif, interval = interval)
    }
    invisible(tms)
  }

convert_tms_to_gif <- function(tms, ints, filepath_gif, interval = 0.5) {
  # if (missing(filepath)) {
  #   stop("Missing filepath.")
  # }

  print_function <- function(tms, ints) {
    i <- 1
    while (i <= length(ints)) {
      int_i <- ints[i]
      # browser()
      purrr::quietly(print(tms[i]))
      # tms[i]
      i <- i + 1
    }
  }

  ani.options("interval" = interval)
  ani.options("autobrowse" = TRUE)
  ani.options("verbose" = FALSE)
  ani.options("ani.width" = 480, "ani.height" = 480)
  # THIS WORKS
  # browser()
  success <- saveGIF(print_function(tms, ints))

  # THIS DOESN'T WORK (EVEN WITH ATTEMPTING TO CREATE TEMPFILE FIRST.)
  # file.create(filepath)
  # conn <- file(filepath, "w")
  # on.exit(close(conn))
  # success <- saveGIF(print_function(tms), movie.name = filepath)

  # THIS DOESN'T WORK
  # filenames <- gsub(paste0(dir, "/"), "", filepaths)
  # success <- animation::im.convert(filenames, output = ".")

  message("Saved gif at ", filepath_gif, ".")
  message("Ignore the error message from Image Magick.")

  # NOTE: THIS IS A WORKAROUND FOR NOT BEING SUCCESSFUL WITH SPECIFYING
  # THE PATH DIRECTLY WITH saveGIF (OR USING im.conver() directly
  # browser()
  file.copy("animation.gif", filepath_gif)
  unlink("animation.gif")
  invisible(success)
}


get_price_idw_raster <- function(spdf, grid_n = 50000, idw_idp = 2.0) {
  grd <-
    tibble::as_data_frame(sp::spsample(spdf, "regular", n = grid_n))
  names(grd) <- c("lon", "lat")
  sp::coordinates(grd) <- c("lon", "lat")
  sp::gridded(grd) <- TRUE
  sp::fullgrid(grd) <- TRUE
  sp::proj4string(grd) <- sp::proj4string(spdf)
  # Since this is written directly in C, not sure how to suprress message.
  # Also, need to not hard-code tis formula.
  idw_spdf <-
    gstat::idw(price ~ 1, spdf, newdata = grd, idp = idw_idp)
  r <- raster::raster(idw_spdf)
  r
}


get_price_idw_raster_trimmed <- function(spdf, spdf_base_map, grid_n = 50000, idw_idp = 2.0) {
  if (missing(spdf_base_map)) {
    spdf_base_map <- import_base_map_spdf()
  }
  idw_spdf <- get_price_idw_raster(spdf, grid_n, idw_idp)
  r <- raster::raster(idw_spdf)
  r_trimmed <- raster::mask(r, spdf_base_map)
  r_m
}

visualize_price_idw <-
  function(spdf,
           col_value = "price",
           spdf_base_map,
           pal_n = 10,
           pal_hex = pal_fixed_hex,
           pal_style = "fixed",
           pal_breaks = pal_fixed_labs,
           points = TRUE,
           point_size = 0.1,
           ...,
           grid_n = 50000,
           idw_idp = 2.0) {
    if (missing(spdf_base_map)) {
      spdf_base_map <- import_base_map_spdf()
    }
    r_trimmed <-
      get_price_idw_raster_trimmed(
        spdf = spdf,
        spdf_base_map = spdf_base_map,
        grid_n = grid_n,
        idw_idp = idw_idp
      )

    tm_layout_base <- get_tm_layout_base()
    tm_layout_pal <- get_tm_layout_pal()
    tm_base_map_dots <-
      get_tm_base_map_dots(spdf_base_map, spdf, points, point_size)
    pal_breaks <- adjust_pal_breaks(pal_breaks, pal_style)

    tm <-
      tm_shape(r_m) +
      tm_raster(
        title = "",
        alpha = 1,
        n = pal_n,
        palette = pal_hex,
        style = pal_style,
        breaks = pal_breaks,
        legend.reverse = TRUE,
        legend.hist = TRUE,
        auto.palette.mapping = FALSE
      ) +
      tm_base_map_dots +
      tm_layout_pal +
      tm_layout_base
    tm
  }
