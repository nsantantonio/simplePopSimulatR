#' randomIntermate function
#'
#' function to (do something)
#'
#' @param genolist [value]
#' @param newpopsize [value]. Default is length(genolist)
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
randomIntermate <- function(genolist, newpopsize = length(genolist)){
	newpop <- list()
	for (k in 1:newpopsize){
		flowers <- sample(1:length(genolist),2)
		newpop[[k]] <- cross(geno1 = genolist[[flowers[1]]], geno2 = genolist[[flowers[2]]])
	}
	return(newpop)
}
