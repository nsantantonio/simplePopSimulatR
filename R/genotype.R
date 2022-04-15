#' genotype function
#'
#' function to genotype a population. 
#'
#' @param pop [value]
#' @param loci [value]
#' @param simplify [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
genotype <- function(pop, loci = NULL, simplify = TRUE) {
	seqMat <- getSeqMatrix(pop)
	if(!is.null(loci)){
		Ml <- list()
		for(i in 1:length(loci)) Ml[[i]] <- seqMat[[i]][, loci[[i]], drop = FALSE]
	} else{
		Ml <- seqMat
	}
	if(simplify) do.call(cbind, Ml) else Ml
}
