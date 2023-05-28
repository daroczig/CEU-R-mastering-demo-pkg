#' Formats number in EUR currency
#' @param x number
#' @return string
#' @export
#' @importFrom scales dollar
#' @examples
#' euro(1000)
#' euro(10.3241245125125)
euro <- function(x) {
    dollar(x, prefix = '€')
}
