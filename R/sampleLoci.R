#' sampleLoci function
#'
#' function to (do something)
#'
#' @param nMarkers [value]
#' @param cM [value]. Default is 100
#' @param nChrom [value]. Default is NULL
#' @param method [value]. Default is "uniform"
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
sampleLoci <- function(nMarkers, cM = 100, nChrom = NULL, method = "uniform", exclude = NULL){
	if (length(nMarkers) > 1 & is.null(nChrom)) nChrom <- length(nMarkers)
	if (length(cM) != nChrom & length(cM) == 1) cM <- rep(cM, nChrom)
	if (length(nMarkers) == 1) rem <- nMarkers %% nChrom else rem = 0
	if (length(nMarkers) == 1 & nChrom > 1) nMarkers <- rep(floor(nMarkers / nChrom), nChrom)
	if (rem > 0) nMarkers[[1]] <- nMarkers[[1]] + rem
	mPos <- list()
	if(method == "uniform"){
		for(i in 1:nChrom) mPos[[i]] <- floor(seq(1, cM[[i]], length.out = nMarkers[[i]])) # uniform sampling
	} else {
		for(i in 1:nChrom) {
			candidates <- 1:cM[[i]]
			if(!is.null(exclude)) {
				if(is.list(exclude)) {
					candidates <- candidates[!candidates %in% exclude[[i]]]
				} else {
					candidates <- candidates[!candidates %in% exclude]
				}
			}
			mPos[[i]] <- sort(sample(candidates, nMarkers[[i]])) # random sampling
		}
	}
	mPos
}
