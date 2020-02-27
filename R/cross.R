#' cross function
#'
#' function to (do something)
#'
#' @param ind1 list. list of a single individual's chromosomes. Chromosomes should be matrices of logical values with 2 coulmns 
#' @param ind2 list. Same as ind1, if ind2 = NULL, then ind1 is crossed to itself (i.e. selfed).
#' @param DH logical. Should a doubled haploid be produced?
#' @param BC logical. Should a doubled haploid be produced?
#' @return returns an individual resulting from a cross of ind1 and ind2. 
#' @details [fill in details here]
#' @examples none
#' @export
cross <- function(ind1, ind2 = NULL, DH = FALSE, BC = FALSE){
	gamete1 <- meiosis(ind1)
	if(DH) {
		gamete2 <- gamete1
	} else if(!BC) {
		gamete2 <- if(is.null(ind2)) meiosis(ind1) else meiosis(ind2)
	} else {
		gamete2 <- NULL
	} 
	pollinate(gamete1, gamete2)
}
