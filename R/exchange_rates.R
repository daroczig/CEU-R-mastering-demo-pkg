#' Look up the value of a US Dollar in EURs
#' @param retried number of times the function already failed
#' @return number
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_error log_info
#' @importFrom checkmate assert_number
get_usdeur <- function(retried = 0) {
    tryCatch({
        ## httr
        usdeur <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=EUR')$rates$EUR
        assert_number(usdeur, lower = 0.9, upper = 1.1)
    }, error = function(e) {
        log_error(e$message)
        if (retried > 3) {
            stop('Gave up')
        }
        Sys.sleep(1 + retried ^ 2)
        get_usdeur(retried = retried + 1)
    })
    log_info('1 USD={usdeur} EUR')
    usdeur
}


#' Look up the current price of a Bitcoin in USD
#' @return number
#' @export
#' @importFrom binancer binance_coins_prices
#' @importFrom logger log_info
#' @importFrom checkmate assert_number
#' @importFrom purrr insistently
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @import data.table
get_bitcoin_price <- memoise(insistently(function() {
  btcusdt <- binance_coins_prices()[symbol == 'BTC', usd]
  assert_number(btcusdt, lower = 1000)
  log_info('The current Bitcoin price is ${btcusdt}')
  btcusdt
}, quiet = FALSE), cache = cache_mem(max_age = 5))


#' Look up the value of a US Dollar in EURs for a date interval
#' @param start_date date
#' @param end_date date
#' @return data.table object with data and value columns
#' @export
#' @importFrom httr content GET
#' @importFrom checkmate assert_numeric
#' @importFrom purrr insistently
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @importFrom data.table data.table
get_usdeurs <- memoise(
  insistently(
    function(start_date = Sys.Date() - 30,
             end_date = Sys.Date()) {
      usdeurs <- content(GET('https://api.exchangerate.host/timeseries',
                             query = list(
                               start_date = start_date,
                               end_date = end_date,
                               base = 'USD',
                               symbols = 'EUR'
                             )))
      usdeurs <- data.table(
        date = as.Date(names(usdeurs$rates)),
        usdeur = as.numeric(unlist(usdeurs$rates)))
      assert_numeric(usdeurs$usdeur, lower = 0.8, upper = 1.2)
      usdeurs
    }, quiet = FALSE), cache = cache_mem(max_age = 5))
