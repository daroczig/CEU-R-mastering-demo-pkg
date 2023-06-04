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
#' @param retried number of times the function already failed
#' @return number
#' @export
#' @importFrom binancer binance_coins_prices
#' @importFrom logger log_error log_info
#' @importFrom checkmate assert_number
#' @import data.table
#' @importFrom purrr insistently
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
get_bitcoin_price <- memoise(
    insistently(
        function() {
            btcusdt <- binance_coins_prices()[symbol == 'BTC', usd]
            assert_number(btcusdt, lower = 1000)
            log_info('The current Bitcoin price is ${btcusdt}')
            btcusdt
        },
        quiet = FALSE),
    cache = cache_mem(max_age = 5))


#' Look up the value of a US Dollar in Euro
#' @param start_date date
#' @param end_date date
#' @return \code{data.table} object with dates and values
#' @export
#' @importFrom httr GET content
#' @importFrom logger log_error log_info
#' @importFrom checkmate assert_numeric
#' @importFrom data.table data.table
#' @importFrom purrr insistently
#' @importFrom memoise memoise
get_usdeurs <- memoise(
    insistently(
        function(start_date = Sys.Date(), end_date = Sys.Date()) {
            response <- GET(
                'https://api.exchangerate.host/timeseries',
                query = list(
                    start_date = start_date,
                    end_date   = end_date,
                    base       = 'USD',
                    symbols    = 'EUR'
                )
            )
            exchange_rates <- content(response)$rates
            usdeur <- data.table(
                date = as.Date(names(exchange_rates)),
                usdeur = as.numeric(unlist(exchange_rates)))
            assert_numeric(usdeur$usdeur, lower = 0.8, upper = 1.2)
            usdeur
        }, quiet = FALSE
    ), cache = cache_mem(max_age = 5)
)
