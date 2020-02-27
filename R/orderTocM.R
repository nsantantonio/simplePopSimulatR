#' orderTocM function
#'
#' function to (do something)
#'
#' @param sol [value]
#' @param Mgrp [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
orderTocM <- function(sol, Mgrp) {
	if (!is.list(Mgrp)) Mgrp <- list(Mgrp)
	cMdfL <- list()
	for (i in 1:length(Mgrp)) {
		cM <- 0
		Mi <- Mgrp[[i]]
		n <- nrow(Mi)
		soli <- sol[[i]]
		for (j in 2:length(soli)) {
			cM <- c(cM, haldane(sum(Mi[, soli[j]] != Mi[, soli[j-1]]) / n))
		}
		cMdfL[[i]] <- data.frame(locus = soli, cM = 100 * cumsum(cM))
	}
	cMdfL
}
