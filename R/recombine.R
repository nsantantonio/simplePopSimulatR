#' recombine function
#'
#' function to (do something)
#'
#' @param chrom [value]
#' @param trackRecomb [value]. Default is FALSE
#' @param force1recomb [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
recombine <- function(chrom, trackRecomb = FALSE, force1recomb = TRUE){
	cM <- nrow(chrom)
	recomb <- c(as.logical(rpois(cM - 1, (cM -1) / 1e4)), FALSE)
	if(force1recomb) while(!any(recomb)) recomb <- c(as.logical(rpois(cM - 1, (cM -1) / 1e4)), FALSE)
	rsite <- which(recomb)
	sisters <- chrom	
	for(i in rsite) sisters[(i + 1):nrow(sisters), ] <- sisters[(i + 1):nrow(sisters), c(2, 1)]
	if(trackRecomb) sisters <- list(sisters = sisters, rsites = rsite)
	sisters
}
