#' makeF1 function
#'
#' function to (do something)
#'
#' @param markerPos [value]
#' @param nChrom [value]. Default is NULL
#' @param cM [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
makeF1 <- function(markerPos, nChrom = NULL, cM = NULL){
	if (!is.list(markerPos)) markerPos <- list(markerPos)
	if (is.null(cM)) cM <- lapply(markerPos, max)
	if (is.null(nChrom)) nChrom <- length(markerPos)
	if (length(cM) != nChrom & length(cM) == 1) cM <- rep(cM, nChrom)
	if (length(markerPos) != nChrom & length(markerPos) == 1) markerPos <- rep(markerPos, nChrom)
	f1 <- list()
	for (i in 1:nChrom) f1[[i]] <- cbind(rep(TRUE, cM[i]), rep(FALSE, cM[i]))
	f1
}
