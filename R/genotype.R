#' genotype function
#'
#' function to (do something)
#'
#' @param markers [value]
#' @param seqMat [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
genotype <- function(seqMat, markers) {
	Ml <- list()
	for(i in 1:length(markers)) Ml[[i]] <- seqMat[[i]][, markers[[i]], drop = FALSE]
	Ml
}
