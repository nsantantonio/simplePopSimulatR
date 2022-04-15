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
mate <- function(genolist, newpopsize = length(genolist)){
	newpop <- list()
	for (k in 1:newpopsize){
		parents <- sample(1:length(genolist),2)
		newpop[[k]] <- cross(ind1 = genolist[[parents[1]]], ind2 = genolist[[parents[2]]])
	}
	return(newpop)
}
