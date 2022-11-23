big_mark <- function(x, ...) {
  if (identical(getOption("OutDec"), ",")) {
    mark <- "."
  } else {
    mark <- ","
  }
  formatC(x,
          big.mark = mark, ...)
}

xml_find_text <- function(x, xpath) {
  out <- x |>
    xml2::xml_find_all(xpath) |>
    xml2::xml_text()

  if (vec_is_empty(out)) {
    out <- vec_init(out)
  }
  out
}

# TODO: Use `xml_clone()` until `xml2::xml_clone()` is provided.
xml_clone <- function(x) {
  xml2::xml_serialize(x, NULL) |>
    xml2::xml_unserialize()
}
