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
get_bitcoin_price <- function(retried = 0) {
    tryCatch({
        btcusdt <- binance_coins_prices()[symbol == 'BTC', usd]
        assert_number(btcusdt, lower = 1000)
        log_info('The current Bitcoin price is ${btcusdt}')
        btcusdt
    },
    error = function(e) {
        log_error(e$message)
        if (retried > 3) {
            stop('Gave up')
        }
        Sys.sleep(1 + retried ^ 2)
        get_bitcoin_price(retried = retried + 1)
    })
}
