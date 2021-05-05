#' Look up the value of a US Dollar in Hungarian Forints
#' @param retried number of times the function already failed
#' @return number
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_error log_info
#' @importFrom checkmate assert_number
get_usdhuf <- function(retried = 0) {
  tryCatch({
    ## httr
    usdhuf <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(usdhuf, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhuf(retried = retried + 1)
  })
  log_info('1 USD={usdhuf} HUF')
  usdhuf
}
