#' antMapR function
#'
#' function to (do something)
#'
#' @param M [value]
#' @param nCore [value]. Default is 1
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
antMapR <- function(M, nCore = 1, ...) {
	if(!is.list(M)) M <- list(M)
	mDist <- lapply(M, function(x) as.matrix(dist(t(x)), upper = TRUE))
	if(nCore > 1 & length(mDist) > 1) {
		require(parallel)
		sol <- mclapply(mDist, antColonyTSP, returnToStart = FALSE, mc.cores = nCore, ...)
	} else {
		# sol <- lapply(mDist, antColonyTSP, returnToStart = FALSE, ...)
		sol <- list()
		for(i in 1:length(M)){
			sol[[i]] <- antColonyTSP(mDist[[i]], returnToStart = FALSE, ...)
		}
	}
	sol
	# Mord <- orderTocM(sol, M)
}
