#' getOrderedM function
#'
#' function to (do something)
#'
#' @param sol [value]
#' @param Mgrp [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
getOrderedM <- function(sol, Mgrp){
	# if(is.list(M)) M <- do.call(cbind, M)
	if(!is.list(sol)) sol <- list(sol)
	Mord <- list()
	for(i in 1:length(sol)) Mord[[i]] <- Mgrp[[i]][, sol[[i]]]
	do.call(cbind, Mord)
}
