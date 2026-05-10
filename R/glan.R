#' Drop brackets
#'
#' @param dropbrackets Drop brackets from a dataset
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- dropbrackets(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropbrackets(col_name))

dropbrackets <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\[|\\]', "", x))
  x
}


#' Drop braces
#'
#' @param dropbraces Drop braces from a dataset
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- dropbraces(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropbraces(col_name))

dropbraces <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\{|\\}', "", x))
  x
}

#' Drop quotation marks
#'
#' @param dropquotes Drop quotation marks from a dataset
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- dropquotes(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropquotes(col_name))

dropquotes <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\"', "", x))
  x
}


#' Drop parentheses
#'
#' @param dropparen Drop parentheses from a dataset
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- dropparen(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropparen(col_name))

dropparen <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\(|\\)', "", x))
  x
}


#' Drop commas
#'
#' @param dropcomma Drop commas from a dataset
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- dropcomma(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropcomma(col_name))

dropcomma <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\,', "", x))
  x
}


#' Drop full stops
#'
#' @param dropstop Drop full stops from a dataset
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- dropstop(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropstop(col_name))

dropstop <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\.', "", x, fixed = TRUE))
  x
}


#' Drop colons
#'
#' @param dropcolon Drop colons from a dataset
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- dropcolon(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropcolon(col_name))

dropcolon <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\:', "", x))
  x
}


#' Drop semicolons
#'
#' @param dropsemi Drop semicolons from a dataset
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- dropsemi(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropsemi(col_name))

dropsemi <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\;', "", x))
  x
}


#' Change to lowercase
#'
#' @param lowerall Change characters to lowercase
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- lowerall(x)
#' @examples x <- x %>% dplyr::mutate(col_name=lowerall(col_name))

lowerall <- function(x) {
  x <- sapply(x, tolower)
  x
}


#' Change to uppercase
#'
#' @param upperall Change characters to uppercase
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- upperall(x)
#' @examples x <- x %>% dplyr::mutate(col_name=upperall(col_name))

upperall <- function(x) {
  x <- sapply(x, toupper)
  x
}


#' Calculate percentage of total (decimal)
#'
#' @param pcdec Calculate percentage of total with decimal output
#' @param x Dataset to which function is applied
#' @param pcdec New column name to be created
#' @param existing_col_name Existing column name to call
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- pcdec(x, new_col_name, existing_col_name)

pcdec <- function(x, pcdec, existing_col_name) {
  x[[pcdec]] <- x[[existing_col_name]]/(sum(x[[existing_col_name]]))
  x
}


#' Calculate percentage of total (rounded decimal)
#'
#' @param percent Calculate percentage of total with *rounded* decimal output (3 digits)
#' @param x Dataset to which function is applied
#' @param percent New column name to be created
#' @param existing_col_name Existing column name to be called
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- percent(x, new_col_name, existing_col_name)

percent <- function(x, percent, existing_col_name) {
  x[[percent]] <- round(x[[existing_col_name]]/(sum(x[[existing_col_name]])), 3)
  x
}


#' Create percentage labels from decimal values
#'
#' @param pclab Create percentage labels for plotting from decimal values
#' @param x Dataset to which function is applied
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- pclab(x)

pclab <- function(x) {
  x$pclab <- gsub(" ", "", paste(x$percent*100, "%"))
  x
}


#' Drop characters or strings contained in a list
#'
#' @param droptext Drop characters or strings contained in a list from a dataset
#' @param x Dataset to which the function is applied
#' @param todrop List of characters or strings to drop from the dataset. Create using: todrop <- paste(c("value1", "value2"...), collapse = "|")
#'
#' @returns Changes are saved to the dataset
#' @export
#'
#' @examples x <- droptext(x)

droptext <- function(x) {
  x[] <- as.data.frame(lapply(x, function(x) gsub(todrop, "", x, ignore.case = TRUE)))
  x
}
