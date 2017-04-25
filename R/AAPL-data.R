#' Price series of Apple Inc. stock
#'
#' The price series of Apple stock is obtained from Yahoo finance.
#' This data set is used to demonstrate several functions in the package such as subgrouping, obtaining a summary of the subgroups, common and special cause partitioning and calculating the \eqn{\sigma} in total, common and special cause data .
#'
#' @docType data
#'
#' @usage data(AAPL)
#'
#'@format An object of class zoo with 3395 rows and 6 columns:
#' \describe{
#'   \item{open}{Opening price}
#'   \item{High}{Daily high price}
#'   \item{Low}{Daily low price}
#'   \item{Close}{Closing price}
#'   \item{AdjClose}{Adjusted closing price for dividends and splits}
#'   \item{Volume}{Trading volume}
#' }
#'
#'
#' @keywords datasets
#'
#' @source \href{https://finance.yahoo.com/quote/AAPL?p=AAPL}{YAHOO Finace}
#'
#' @examples
#' data(AAPL)
#' head(AAPL)
#' ClosePrice=AAPL[,"Close"]
"AAPL"


