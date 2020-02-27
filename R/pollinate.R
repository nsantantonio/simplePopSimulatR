#' pollinate function
#'
#' Make individual from gametes.
#'
#' @param gamete1 list. List of haploid chromosomes, where chromosomes have 1 column.
#' @param gamete2 list. Same as gamete1. If gamete2 = NULL, then 
#' @return a list of chromosomes for 1 individual formed from the gametes provided.
#' @details none
#' @examples none
#' @export
pollinate <- function(gamete1, gamete2 = NULL){
	seed <- list()
	if(is.null(gamete2)) gamete2 <- lapply(gamete1, function(x) rep(FALSE, length(x))) 
	for (i in 1:length(gamete1)) seed[[i]] <- cbind(gamete1[[i]], gamete2[[i]])
	seed
}
