#' getCO function
#'
#' function to (do something)
#'
#' @param X [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
getCO <- function(X){ # need wrapper function
	COl <- list() 
	for(i in 2:ncol(X)) COl[[i - 1]] <- X[, i - 1] != X[, i]
	do.call(cbind, COl)
}
