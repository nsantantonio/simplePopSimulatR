#' plotLD function
#'
#' function to (do something)
#'
#' @param M [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
plotLD <- function(M){
	if(is.list(M)) M <- do.call(cbind, M) 	
	rho <- cor(M)^2
	image(rho[, ncol(rho):1], col = pal(20))
}
