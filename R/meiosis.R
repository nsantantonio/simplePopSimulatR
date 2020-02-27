#' meiosis function
#'
#' function to (do something)
#'
#' @param geno [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
meiosis <- function(geno){
	chromatids <- lapply(geno, recombine)
	gamete <- lapply(chromatids, function(x) x[, sample(1:2,1)])
	gamete
}
