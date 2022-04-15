#' makePop function
#'
#' function to (do something)
#'
#' @param f1 [value]
#' @param popSize [value]. Default is 100
#' @param type [value]. Default is "BC"
#' @param gen [value]. Default is 6
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
makePop <- function(f1, popSize = 100, type = "BC", gen = 6){
	dh <- if(type == "DH") TRUE else FALSE
	bc <- if(type == "BC") TRUE else FALSE
	pop <- lapply(1:popSize, function(x) cross(f1, DH = dh, BC = bc))
	if(type %in% c("RIL", "outcross")){
		f <- 2
		while(f <= gen){
			pop <- if(type == "RIL") lapply(pop, cross) else mate(pop)
			f <- f + 1
		}	
	}
	pop
}
