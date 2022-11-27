#' @export
wmts_tiles <- function(x, wmts,
                       zoom = NULL,
                       zoom_in = 0L,
                       max_tiles = 500L) {
  bbox <- bbox_wmts_tiles(x)
  query <- query_wmts_tiles(bbox)

  tile_matrix_set <- attr(wmts, "tile_matrix_set") |>
    dplyr::select("identifier", "zoom_min", "zoom_max")

  wmts <- tibble::as_tibble(wmts) |>
    dplyr::select("title", "tile_matrix_set", "resource_url") |>
    dplyr::left_join(tile_matrix_set,
                     by = c("tile_matrix_set" = "identifier")) |>
    dplyr::select(!"tile_matrix_set") |>
    tibble::add_column(zoom = zoom %||% query$zoom + zoom_in) |>
    dplyr::mutate(zoom = .data$zoom |>
                    pmax(.data$zoom_min) |>
                    pmin(.data$zoom_max)) |>
    dplyr::select(!c("zoom_min", "zoom_max")) |>
    dplyr::left_join(query$query,
                     by = "zoom")

  max_tiles <- floor(max_tiles)
  total_tiles <- sum(wmts$total_tiles)

  if (total_tiles > max_tiles) {
    abort(c("Total number of tiles must not exceed `max_tiles`.",
            "*" = stringr::str_glue("{big_mark(total_tiles)} tiles in total."),
            "i" = stringr::str_glue("max_tiles = {big_mark(max_tiles)}")))
  }

  pb <- progress::progress_bar$new(total = vec_size(wmts))
  out <- wmts |>
    dplyr::select(!"total_tiles") |>
    dplyr::rowwise() |>
    dplyr::mutate(tiles = list({
      out <- bbox |>
        get_wmts_tiles(resource_url = resource_url,
                       zoom = zoom)
      pb$tick()
      out
    })) |>
    dplyr::ungroup() |>
    dplyr::select(!c("resource_url"))

  stickyr::new_sticky_tibble(out,
                             cols = c("title", "tiles"),
                             class = "wmts_tiles",
                             class_grouped_df = "wmts_tiles",
                             class_rowwise_df = "wmts_tiles")
}

#' @export
write_wmts_tiles <- function(x, path,
                             ext = c("tif", "png", "jpg", "gif"),
                             file_name = x$title,
                             text = x$title,
                             size = 20,
                             boxcolor = "white",
                             fps = 1, ...) {
  args <- list2(...)
  ext <- arg_match(ext, c("tif", "png", "jpg", "gif"))

  if (ext == "gif" || fs::path_ext(path) == "gif") {
    if (fs::path_ext(path) != "gif") {
      abort('`path` extension must be "gif".')
    }

    x <- x |>
      dplyr::rowwise() |>
      dplyr::mutate(file = fs::file_temp(ext = "png"))

    x |>
      dplyr::group_walk(function(x, y) {
        terra::writeRaster(x$tiles[[1L]], x$file,
                           overwrite = TRUE)
      })

    args_annotate <- purrr::compact(args[fn_fmls_names(magick::image_annotate)])
    args_animate <- purrr::compact(args[fn_fmls_names(magick::image_animate)])
    args_write <- purrr::compact(args[fn_fmls_names(magick::image_write)])

    x$file |>
      magick::image_read() |>
      purrr::partial(magick::image_annotate,
                     text = text,
                     size = size,
                     boxcolor = boxcolor,
                     !!!args_annotate)() |>
      purrr::partial(magick::image_animate,
                     fps = fps,
                     !!!args_animate)() |>
      magick::image_write(path)
  } else {
    fs::dir_create(path)

    x |>
      tibble::add_column(file_name = file_name) |>
      dplyr::rowwise() |>
      dplyr::group_walk(function(x, y) {
        file <- fs::path(path, x$file_name,
                         ext = ext)

        terra::writeRaster(x$tiles[[1L]], file,
                           overwrite = TRUE, ...)
      })
  }
  invisible()
}

#' @export
tbl_sum.wmts_tiles <- function(x) {
  out <- NextMethod()
  names(out)[[1L]] <- "WMTS tiles"
  out
}

bbox_wmts_tiles <- function(x) {
  crs <- 4326L

  bbox <- sf::st_bbox(x)

  if (is.na(sf::st_crs(bbox))) {
    sf::st_crs(bbox) <- crs
  } else {
    bbox <- bbox |>
      sf::st_as_sfc() |>
      sf::st_transform(crs) |>
      sf::st_bbox()
  }
  bbox
}

query_wmts_tiles <- function(bbox) {
  zoom_levels <- 0:20
  min_tiles <- 5L

  query <- slippymath::bbox_tile_query(bbox,
                                       zoom_levels = zoom_levels) |>
    dplyr::select("total_tiles", "zoom") |>
    dplyr::mutate(total_tiles = as.integer(total_tiles),
                  zoom = as.integer(zoom))

  zoom <- query$zoom[query$total_tiles >= min_tiles] |>
    dplyr::first(default = max(zoom_levels))

  list(query = query,
       zoom = zoom)
}

get_wmts_tiles <- function(bbox, resource_url, zoom) {
  resource_url <- resource_url |>
    dplyr::filter(.data$format %in% c("image/png", "image/jpeg", "png", "jpeg"))

  if (vec_is_empty(resource_url)) {
    out <- NULL
  } else {
    resource_url <- resource_url |>
      dplyr::slice(1L)

    provider <- list(src = "",
                     q = resource_url$template,
                     sub = resource_url$subdomain,
                     cit = "")
    out <- purrr::safely(maptiles::get_tiles)(bbox,
                                              provider = provider,
                                              zoom = zoom,
                                              crop = TRUE,
                                              cachedir = fs::file_temp()) |>
      purrr::chuck("result")
    Sys.sleep(1)
  }
  out
}
