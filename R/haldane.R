#' haldane function
#'
#' function to (do something)
#'
#' @param r [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
haldane <- function(r) { if(r < 0.5 & r >= 0) -0.5 * log(1 - 2*r) else 20 }
