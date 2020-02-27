#' clusterM function
#'
#' function to (do something)
#'
#' @param M [value]
#' @param nExpChr [value]
#' @param method [value]. Default is "hclust"
#' @param nK [value]. Default is 1
#' @param makePlots [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
clusterM <- function(M, nExpChr, method = "hclust", nK = 1, makePlots = TRUE) {
	if(is.list(M)) M <- do.call(cbind, M) 
	if(!is.matrix(M)) stop("Please provide a single matrix or a list of matrices with markers on columns and individuals on rows!")
	Mt <- t(M)
	if(method == "kmeans") {
		kgrp <- kmeans(Mt, nExpChr)
		# kgrp <- kmeans(dist(Mt), nExpChr)
		clust <- kgrp$cluster		
		# kmsol <- list()
		# for(i in 1:nK) kmsol[[i]] <- 
		# 
		# groups <- lapply(clust, function(x) split(1:ncol(M), x))
		# lapply(clust, table)

		# plotLD(M[, unlist(groups[[1]])])
		# split(1:ncol(M), clust[[1]])
		# split(1:ncol(M), clust[[2]])

	} else if (method == "hclust"){
		tree <- hclust(dist(Mt), method = "single")
		if(makePlots) plot(tree, xlab = "markers")
		clust <- cutree(tree, k = nExpChr)
	} else {
		stop("please provide method!")
	}

	groups <- split(1:ncol(M), clust)
	Mg <- lapply(groups, function(x) M[, x])

	if(makePlots) plotLD(do.call(cbind, Mg))
	Mg
}
