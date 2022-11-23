wmts_tiles <- function(x, wmts,
                       zoom = NULL,
                       max_tiles = 1000L) {
  bbox <- bbox_wmts_tiles(x)
  query <- query_wmts_tiles(bbox)

  tile_matrix_set <- attr(wmts, "tile_matrix_set") |>
    dplyr::select("identifier", "zoom_min", "zoom_max")

  wmts <- tibble::as_tibble(wmts) |>
    dplyr::select("title", "tile_matrix_set", "resource_url") |>
    dplyr::left_join(tile_matrix_set,
                     by = c("tile_matrix_set" = "identifier")) |>
    dplyr::select(!"tile_matrix_set") |>
    tibble::add_column(zoom = zoom %||% query$zoom) |>
    dplyr::mutate(zoom = .data$zoom |>
                    pmax(.data$zoom_min) |>
                    pmin(.data$zoom_max)) |>
    dplyr::select(!c("zoom_min", "zoom_max")) |>
    dplyr::left_join(query$query,
                     by = "zoom")

  if (sum(wmts$total_tiles) > max_tiles) {
    abort("Total number of tiles must not exceed `max_tiles`.")
  }
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
    dplyr::select("total_tiles", "zoom")

  zoom <- query$zoom[query$total_tiles >= min_tiles] |>
    dplyr::first(default = max(zoom_levels))

  list(query = query,
       zoom = zoom)
}
