#' Read WMTS
#'
#' @export
read_wmts <- function(x, ...) {
  wmts <- xml2::read_xml(x, ...)

  layer <- read_layer_wmts(wmts)
  tile_matrix_set <- read_tile_matrix_set(wmts)

  stickyr::new_sticky_tibble(layer,
                             cols = c("layer_id", "title", "tile_matrix_set", "resource_url"),
                             col_show = !c("layer_id", "tile_matrix_set", "resource_url"),
                             attrs = c("wmts", "tile_matrix_set"),
                             wmts = wmts,
                             tile_matrix_set = tile_matrix_set,
                             class = "wmts",
                             class_grouped_df = "wmts",
                             class_rowwise_df = "wmts")
}

#' @export
tbl_sum.wmts <- function(x) {
  out <- NextMethod()
  names(out)[[1L]] <- "WMTS"
  out
}

#' @export
collect.wmts <- function(x, ...) {
  wmts <- xml_clone(attr(x, "wmts"))

  # Update layers
  layer <- wmts |>
    xml2::xml_find_all("d1:Contents/d1:Layer")
  layer[x$layer_id] |>
    xml2::xml_set_text(x$title)
  xml2::xml_remove(layer[vec_as_location(-x$layer_id, length(layer))])

  # Update Tile Matrices
  tile_matrix_set_id <- attr(x, "tile_matrix_set") |>
    dplyr::filter(.data$identifier %in% x$tile_matrix_set) |>
    dplyr::pull("tile_matrix_set_id")
  tile_matrix_set <- wmts |>
    xml2::xml_find_all("d1:Contents/d1:TileMatrixSet")
  xml2::xml_remove(tile_matrix_set[vec_as_location(-tile_matrix_set_id, length(tile_matrix_set))])

  wmts
}

read_layer_wmts <- function(wmts) {
  layer <- wmts |>
    xml2::xml_find_all("d1:Contents/d1:Layer")
  layer_id <- seq_along(layer)
  xml2::xml_attr(layer, "layer_id") <- layer_id

  data_layer <- tibble::tibble(layer_id = layer_id,
                               title = layer |>
                                 xml_find_text("ows:Title"),
                               identifier = layer |>
                                 xml_find_text("ows:Identifier"),
                               style = layer |>
                                 xml_find_text("d1:Style/ows:Identifier"),
                               format = layer |>
                                 xml_find_text("d1:Format"),
                               tile_matrix_set = layer |>
                                 xml_find_text("d1:TileMatrixSetLink/d1:TileMatrixSet"))

  # resource_url
  resource_url <- layer |>
    xml2::xml_find_all("d1:ResourceURL")

  template <- resource_url |>
    xml2::xml_attr("template") |>
    stringr::str_replace_all(c("\\{TileMatrix\\}" = "{z}",
                               "\\{TileCol\\}" = "{x}",
                               "\\{TileRow\\}" = "{y}"))

  suffix_template <- urltools::suffix_extract(urltools::domain(template))
  urltools::domain(template) <- stringr::str_c("{s}", suffix_template$domain, suffix_template$suffix,
                                               sep = ".")

  data_resource_url <- tibble::tibble(layer_id = resource_url |>
                                        purrr::map_chr(function(x) {
                                          xml2::xml_parent(x) |>
                                            xml2::xml_attr("layer_id")
                                        }) |>
                                        as.integer(),
                                      format = resource_url |>
                                        xml2::xml_attr("format"),
                                      template = template,
                                      subdomain = suffix_template$subdomain,
                                      resource_type = resource_url |>
                                        xml2::xml_attr("resourceType")) |>
    # Nest
    dplyr::group_by(.data$layer_id, .data$format, .data$resource_type, .data$template) |>
    dplyr::summarise(subdomain = list(.data$subdomain),
                     .groups = "drop") |>

    # Replace {Stype} to `style`
    dplyr::left_join(data_layer |>
                       dplyr::select("layer_id", "style"),
                     by = "layer_id") |>
    dplyr::mutate(template = template |>
                    stringr::str_replace("\\{Style\\}", .data$style)) |>
    dplyr::select(!"style") |>
    dplyr::group_nest(.data$layer_id,
                      .key = "resource_url")

  xml2::xml_attr(layer, "layer_id") <- NULL

  data_layer |>
    dplyr::left_join(data_resource_url,
                     by = "layer_id")
}

read_tile_matrix_set <- function(wmts) {
  tile_matrix_set <- wmts |>
    xml2::xml_find_all("d1:Contents/d1:TileMatrixSet")
  tile_matrix_set_id <- seq_along(tile_matrix_set)
  xml2::xml_attr(tile_matrix_set, "tile_matrix_set_id") <- tile_matrix_set_id

  time_matrix <- read_tile_matrix(tile_matrix_set)
  xml2::xml_attr(tile_matrix_set, "tile_matrix_set_id") <- NULL

  tibble::tibble(tile_matrix_set_id = tile_matrix_set_id,
                 identifier = tile_matrix_set |>
                   xml_find_text("ows:Identifier"),
                 supported_crs = tile_matrix_set |>
                   xml_find_text("ows:SupportedCRS")) |>
    dplyr::left_join(time_matrix,
                     by = "tile_matrix_set_id") |>

    # Add `zoom_min`, `zoom_max`
    dplyr::rowwise() |>
    dplyr::mutate(zoom_min = min(tile_matrix$identifier),
                  zoom_max = max(tile_matrix$identifier)) |>
    dplyr::ungroup()
}

read_tile_matrix <- function(tile_matrix_set) {
  tile_matrix <- tile_matrix_set |>
    xml2::xml_find_all("d1:TileMatrix")

  tibble::tibble(tile_matrix_set_id = tile_matrix |>
                   purrr::map_chr(function(x) {
                     xml2::xml_parent(x) |>
                       xml2::xml_attr("tile_matrix_set_id")
                   }) |>
                   as.integer(),
                 identifier = tile_matrix |>
                   xml_find_text("ows:Identifier") |>
                   as.integer(),
                 scale_denominator = tile_matrix |>
                   xml_find_text("ScaleDenominator") |>
                   as.double(),
                 top_left_corner = tile_matrix |>
                   xml_find_text("TopLeftCorner") |>
                   stringr::str_split("\\s") |>
                   purrr::map(as.double),
                 tile_width = tile_matrix |>
                   xml_find_text("TileWidth") |>
                   as.integer(),
                 tile_height = tile_matrix |>
                   xml_find_text("TileHeight") |>
                   as.integer(),
                 matrix_width = tile_matrix |>
                   xml_find_text("MatrixWidth") |>
                   as.integer(),
                 matrix_height = tile_matrix |>
                   xml_find_text("MatrixHeight") |>
                   as.integer()) |>
    dplyr::group_nest(tile_matrix_set_id,
                      .key = "tile_matrix")
}
