#' Drop Brackets
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples x <- dropbrackets(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropbrackets(col_name))

dropbrackets <- function(x) {
  x[] <- lapply(x, function(x) gsub("\\[|\\]", "", x))
  x
}

# Drop braces ("{" AND "}") from dataframe
dropbraces <- function(x) {
  x[] <- lapply(x, function(x) gsub("\\{|\\}", "", x))
  x
}

# Drop quotes ('"') from dataframe (Requires stringr & dplyr packages)
dropquotes <- function(x) {
  library(dplyr)
  library(stringr)
  x <- x %>%
    mutate(across(everything(), ~str_remove_all(., c('"'))))
  x
}

