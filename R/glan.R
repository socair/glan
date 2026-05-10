#' Bain lúibíní cearnacha
#'
#' @param dropbrackets Bain lúibíní cearnacha as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropbrackets(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropbrackets(col_name))

dropbrackets <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\[|\\]', "", x))
  x
}


#' Bain lúibíní slabhracha
#'
#' @param dropbraces Bain lúibíní slabhracha as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropbraces(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropbraces(col_name))

dropbraces <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\{|\\}', "", x))
  x
}

#' Bain comhartha athfhriotail
#'
#' @param dropquotes Bain comhartha athfhriotail as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropquotes(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropquotes(col_name))

dropquotes <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\"', "", x))
  x
}


#' Bain lúibíní
#'
#' @param dropparen Bain lúibíní as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropparen(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropparen(col_name))

dropparen <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\(|\\)', "", x))
  x
}


#' Bain camóga
#'
#' @param dropcomma Bain camóga as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropcomma(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropcomma(col_name))

dropcomma <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\,', "", x))
  x
}


#' Bain lánstadanna
#'
#' @param dropstop Bain lánstadanna as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropstop(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropstop(col_name))

dropstop <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\.', "", x, fixed = TRUE))
  x
}


#' Bain idirstadanna
#'
#' @param dropcolon Bain idirstadanna as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropcolon(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropcolon(col_name))

dropcolon <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\:', "", x))
  x
}


#' Bain leathstadanna
#'
#' @param dropsemi Bain leathstadanna as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropsemi(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropsemi(col_name))

dropsemi <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\;', "", x))
  x
}


#' Athraigh go litreacha beaga
#'
#' @param lowerall Athraigh an téacs go litreacha beaga
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- lowerall(x)
#' @examples x <- x %>% dplyr::mutate(col_name=lowerall(col_name))

lowerall <- function(x) {
  x <- sapply(x, tolower)
  x
}


#' Athraigh go litreacha móra
#'
#' @param upperall Athraigh an téacs go litreacha móra
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- upperall(x)
#' @examples x <- x %>% dplyr::mutate(col_name=upperall(col_name))

upperall <- function(x) {
  x <- sapply(x, toupper)
  x
}


#' Ríomh céatadán den iomlán (deachúil)
#'
#' @param pcdec Ríomh céatadán den iomlán le haschur deachúil
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#' @param pcdec New column name to be created
#' @param existing_col_name Existing column name to call
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- pcdec(x, new_col_name, existing_col_name)

pcdec <- function(x, pcdec, existing_col_name) {
  x[[pcdec]] <- x[[existing_col_name]]/(sum(x[[existing_col_name]]))
  x
}


#' Ríomh céatadán den iomlán (deachúil cruinn)
#'
#' @param percent Ríomh céatadán den iomlán le haschur deachúil *cruinn* (3 dhigit)
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#' @param percent New column name to be created
#' @param existing_col_name Existing column name to be called
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- percent(x, new_col_name, existing_col_name)

percent <- function(x, percent, existing_col_name) {
  x[[percent]] <- round(x[[existing_col_name]]/(sum(x[[existing_col_name]])), 3)
  x
}


#' Cruthaigh lipéid céatadáin ó luachanna deachúlacha
#'
#' @param pclab Lipéid chéatadáin a chruthú le haghaidh plota ó luachanna deachúlacha
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- pclab(x)

pclab <- function(x) {
  x$pclab <- gsub(" ", "", paste(x$percent*100, "%"))
  x
}


#' Bain carachtair nó teaghráin atá i liosta
#'
#' @param droptext Bain carachtair nó teaghráin atá i liosta as tacar sonraí
#' @param x An tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#' @param todrop Liosta carachtar nó teaghrán le baint as an tacar sonraí: todrop <- paste(c("value1", "value2"...), collapse = "|")
#'
#' @returns Sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- droptext(x)

droptext <- function(x) {
  x[] <- as.data.frame(lapply(x, function(x) gsub(todrop, "", x, ignore.case = TRUE)))
  x
}
