#' bain lúibíní cearnacha
#'
#' @param dropbrackets bain lúibíní cearnacha as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropbrackets(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropbrackets(col_name))

dropbrackets <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\[|\\]', "", x))
  x
}


#' bain lúibíní slabhracha
#'
#' @param dropbraces bain lúibíní slabhracha as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropbraces(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropbraces(col_name))

dropbraces <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\{|\\}', "", x))
  x
}

#' bain comhartha athfhriotail
#'
#' @param dropquotes bain comhartha athfhriotail as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropquotes(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropquotes(col_name))

dropquotes <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\"', "", x))
  x
}


#' bain lúibíní
#'
#' @param dropparen bain lúibíní as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropparen(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropparen(col_name))

dropparen <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\(|\\)', "", x))
  x
}


#' bain camóga
#'
#' @param dropcomma bain camóga as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropcomma(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropcomma(col_name))

dropcomma <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\,', "", x))
  x
}


#' bain lánstadanna
#'
#' @param dropstop bain lánstadanna as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropstop(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropstop(col_name))

dropstop <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\.', "", x, fixed = TRUE))
  x
}


#' bain idirstadanna
#'
#' @param dropcolon bain idirstadanna as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropcolon(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropcolon(col_name))

dropcolon <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\:', "", x))
  x
}


#' bain leathstadanna
#'
#' @param dropsemi bain leathstadanna as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropsemi(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropsemi(col_name))

dropsemi <- function(x) {
  x[] <- lapply(x, function(x) gsub('\\;', "", x))
  x
}


#' bain spásanna
#'
#' @param dropspaces bain spásanna as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropspaces(x)
#' @examples x <- x %>% dplyr::mutate(col_name=dropspaces(col_name))

dropspaces <- function(x) {
  x[] <- lapply(x, function(x) gsub(' ', "", x))
  x
}

#' athraigh go litreacha beaga
#'
#' @param lowerall athraigh an téacs go litreacha beaga
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- lowerall(x)
#' @examples x <- x %>% dplyr::mutate(col_name=lowerall(col_name))

lowerall <- function(x) {
  x <- sapply(x, tolower)
  x
}


#' athraigh go litreacha móra
#'
#' @param upperall athraigh an téacs go litreacha móra
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- upperall(x)
#' @examples x <- x %>% dplyr::mutate(col_name=upperall(col_name))

upperall <- function(x) {
  x <- sapply(x, toupper)
  x
}


#' ríomh céatadán den iomlán (deachúil)
#'
#' @param pcdec ríomh céatadán den iomlán le haschur deachúil
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#' @param pcdec ainm nua colúin le cruthú
#' @param existing_col_name ainm colúin atá ann cheana féin le glaoch
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- pcdec(x, new_col_name, existing_col_name)

pcdec <- function(x, pcdec, existing_col_name) {
  x[[pcdec]] <- x[[existing_col_name]]/(sum(x[[existing_col_name]]))
  x
}


#' ríomh céatadán den iomlán (deachúil cruinn)
#'
#' @param percent ríomh céatadán den iomlán le haschur deachúil *cruinn* (3 dhigit)
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#' @param percent ainm nua colúin le cruthú
#' @param existing_col_name ainm colúin atá ann cheana féin le glaoch
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- percent(x, new_col_name, existing_col_name)

percent <- function(x, percent, existing_col_name) {
  x[[percent]] <- round(x[[existing_col_name]]/(sum(x[[existing_col_name]])), 3)
  x
}


#' cruthaigh lipéid céatadáin ó luachanna deachúlacha
#'
#' @param pclab lipéid chéatadáin a chruthú le haghaidh plota ó luachanna deachúlacha
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- pclab(x)

pclab <- function(x) {
  x$pclab <- gsub(" ", "", paste(x$percent*100, "%"))
  x
}


#' bain carachtair nó teaghráin atá i liosta
#'
#' @param droptext bain carachtair nó teaghráin atá i liosta as tacar sonraí
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#' @param todrop liosta carachtar nó teaghrán le baint as an tacar sonraí: todrop <- paste(c("value1", "value2"...), collapse = "|")
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- droptext(x)

droptext <- function(x) {
  x[] <- as.data.frame(lapply(x, function(x) gsub(todrop, "", x, ignore.case = TRUE)))
  x
}


#' athraigh na chéad sraitheanna
#'
#' @param firstrow athraigh luachanna an chéad ró go hainmneacha na gcolún
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- firstrow(x)

firstrow <- function(x) {
  x <- x %>% row_to_names(row_number=1)
  x
}


#' bain colúin NA
#'
#' @param dropnacol bain colúin ina bhfuil luachanna NA
#' @param x an tacar sonraí ar a gcuirtear an fheidhm i bhfeidhm
#'
#' @returns sábháiltear athruithe sa tacar sonraí
#' @export
#'
#' @examples x <- dropnacol(x)

dropnacol <- function(x) {
  x <- x[,colMeans(is.na(x)) !=1]
  x
}

