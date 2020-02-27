#' recSel function
#'
#' function to (do something)
#'
#' @param R [value]
#' @param N [value]
#' @param method [value]. Default is "uniRec"
#' @param tR [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
recSel <- function(R, N, method = "uniRec", tR = TRUE){
	if(is.list(R)){
		if(tR) R <- lapply(R, t)
		R <- do.call(rbind, R)
	} else {
		if(tR) R <- t(R)
	}
	cM <- haldane(rowSums(R) / ncol(R)) 
	if(is.logical(R)) class(R) <- "numeric"
	cat(nrow(R), "markers scored on", ncol(R), "projeny\n")
	if(method == "maxRec"){
		cdotj <- colSums(R)
		sel <- order(-cdotj)[1:N]
	} else if (method == "uniRec"){
		m <- (matrix(1, 1, ncol(R)) %x% cM)
		d <- R / m
		ddotj <- colSums(d)
		sel <- which.max(ddotj)
		n <- 1
		while(n < N){
			S <- d[,sel, drop = FALSE]
			didot <- rowSums(S)
			p <- max(didot)
			R[, sel] <- 0
			tj <- (p - didot) %*% R
			sel <- c(sel, which.max(tj))
			n <- length(sel) 
		}
	} else {
		cat("Please specify 'uniRec' or 'maxRec' as the argument to method!\n")
	}
	sel
}
