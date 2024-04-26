#' @name nafuns
#' @aliases propor
#' @aliases sumna
#' @aliases meanna
#' @aliases maxna
#' @aliases minna
#'
#' @title Common functions that handle na.rm = TRUE but return NA instead of 0 when all NA
#' @param x vector
#' @param na.rm standard na.rm behavior. Default TRUE unlike the base functions
#'
NULL

#' @rdname nafuns
#' @export
propor <- function(x, na.rm = TRUE) {
  sumna(x, na.rm = na.rm)/length(x)
}

# when everything being summed (or otherwise aggregated) is NA, sum(na.rm =
# TRUE) gives 0, but I need it to be NA. So define a function on the backend,
# some of the code expects a na.rm so pass it I guess really, should make these
# generic and accept the FUN, but not now

#' @rdname nafuns
#' @export
sumna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
}

#' @rdname nafuns
#' @export
meanna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE))
}

#' @rdname nafuns
#' @export
maxna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
}

#' @rdname nafuns
#' @export
minna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
}
