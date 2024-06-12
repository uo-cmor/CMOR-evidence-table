scaleCoords <- shiny:::scaleCoords
panelMatch <- shiny:::panelMatch
asNumber <- shiny:::asNumber

nearBars <- function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL, 
					panelvar2 = NULL, threshold = 0, maxpoints = NULL, addDist = FALSE, 
					allRows = FALSE) 
{
	if (is.null(coordinfo$x) | is.null(coordinfo$y)) {
		if (addDist) 
			df$dist_ <- NA_real_
		if (allRows) 
			df$selected_ <- FALSE
		else df <- df[0, , drop = FALSE]
		return(df)
	}
	if (is.null(coordinfo$x)) {
		stop("nearBars requires a click/hover/double-click object with x and y values.")
	}
	xvar <- xvar %||% coordinfo$mapping$x
	yvar <- yvar %||% coordinfo$mapping$y
	panelvar1 <- panelvar1 %||% coordinfo$mapping$panelvar1
	panelvar2 <- panelvar2 %||% coordinfo$mapping$panelvar2
	if (is.null(xvar)) 
		stop("nearBars: not able to automatically infer `xvar` from coordinfo")
	if (is.null(yvar)) 
		stop("nearBars: not able to automatically infer `yvar` from coordinfo")
	x <- asNumber(df[[xvar]])
	y <- asNumber(df[[yvar]])
	coordPx <- scaleCoords(coordinfo$x, coordinfo$y, coordinfo)
	dataPx <- scaleCoords(x, y, coordinfo)
	dist_cat <- abs(y - coordinfo$y)
	dist_val <- coordPx$x - dataPx$x
	if (addDist) 
		df$dist_ <- dist_cat
	keep_rows <- (dist_val <= threshold & coordinfo$x >= 0 & dist_cat <= 0.5)
	if (!is.null(panelvar1)) 
		keep_rows <- keep_rows & panelMatch(coordinfo$panelvar1, 
																				df[[panelvar1]])
	if (!is.null(panelvar2)) 
		keep_rows <- keep_rows & panelMatch(coordinfo$panelvar2, 
																				df[[panelvar2]])
	keep_idx <- which(keep_rows)
	dists <- dist_cat[keep_idx]
	keep_idx <- keep_idx[order(dists)]
	if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
		keep_idx <- keep_idx[seq_len(maxpoints)]
	}
	if (allRows) {
		df$selected_ <- FALSE
		df$selected_[keep_idx] <- TRUE
	}
	else {
		df <- df[keep_idx, , drop = FALSE]
	}
	df
}
