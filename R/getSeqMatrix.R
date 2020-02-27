#' getSeqMatrix function
#'
#' function to (do something)
#'
#' @param alCounts [value]
#' @param chr [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
getSeqMatrix <- function(alCounts, chr = NULL){
	if(class(alCounts[[1]][[1]][[1]]) == "logical") alCounts <- getSeq(alCounts)
	if(is.null(chr)) chr <- 1:length(alCounts[[1]])
	gmat <- list()
	for(i in chr){
		gmat[[i]] <- do.call(rbind, lapply(alCounts, "[[", i))
	}
	gmat
}
