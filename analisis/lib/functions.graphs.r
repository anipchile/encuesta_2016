#> -------------------------------------------------------------------------------------------------------------------------
#> grph.getBinomialCIRadius(data, format = 'AbsOverTotal', alpha = .05)
#>
#> Calculates a binomial confidence interval based on the normal approximation to the binomial distribution, as described in
#> http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
#>
#> data is matrix(ncol==2)
#> format is 'AbsOverTotal', 'AbsFreqs', or 'RateOverTotal'.
#> format=='AbsOverTotal' means mydata[,1] is an absolute number of 'Yes', and mydata[,2] is the total (i.e., 'Yes'+'No')
#> format=='AbsFreqs' means mydata[,1] is an absolute number of 'Yes', and mydata[,2] is an absolute number of 'No'
#> format=='RateOverTotal' means 0<=mydata[,1]<=1, and mydata[,2] is the total (i.e., 'Yes'+'No')
#> -------------------------------------------------------------------------------------------------------------------------
grph.getBinomialCIRadius = function(mydata, format='AbsOverTotal', alpha=.05) {
	#> Preconditions
	if (ncol(mydata)!=2)
		stop(paste('grph.getBinomialCIRadius(arg1) expects arg1[N,2]: arg1 has [', nrow(arg1), ',', ncol(arg1), ']', sep=''))
	if (alpha<.0001 | alpha>.5)
		warning('provided alpha was less than .0001 or more than .5')
	if (!is.numeric(mydata))
		stop('grph.getBinomialCIRadius(arg1) expects that arg1 is numeric. It is not.')

	if (format=='AbsOverTotal') {
		mydata[,1] = abs(mydata[,1]/mydata[,2])
	} else if (format=='AbsFreqs') {
		mydata[,2] = mydata[,1] + mydata[,2]
		mydata[,1] = abs(mydata[,1]/mydata[,2])
	} else if (format=='RateOverTotal') {
		if (sum(mydata[,1]>1)>0)
			stop("grph.getBinomialCIRadius with format='RateOverTotal' expects 0<mydata[,1]<1")
		mydata[,1] = abs(mydata[,1])
	} else {
		stop("grph.getBinomialCIRadius expects format==('AbsFreqs' | 'AbsOverTotal' | 'RateOverTotal')")
	}
	return(qnorm(1-alpha/2) * sqrt(abs(mydata[,1]) * (1-abs(mydata[,1])) / mydata[,2]))
}

#> -------------------------------------------------------------------------------------------------------------------------
#> grph.drawConfIntervals(data, coord, deltaless, deltaplus = deltaless, horizontal = TRUE)
#>
#> Given an open plot, it draws confidence intervals on it. It does not invoke grph.getBinomialCIRadius() to calculate the
#> width of the confidence intervals (you should provide the limits of the interval through deltaless and deltaplus).
#>
#> data should be an array or list such that 0<=data[i]<=1 for all i.
#> coord should be the coordinates of the other dimension (e.g.,if horizontal bars, then the yc coordinates).
#> deltaless should be the left (or bottom) limit of the interval.
#> deltaplus should be the right (or top) limit of the interval.
#> horizontal is TRUE if the bars are horizontal.
#> -------------------------------------------------------------------------------------------------------------------------
grph.drawConfIntervals = function(mydata, coord, deltaless, deltaplus = deltaless, horizontal = TRUE) {
	if (horizontal)
		arrows(mydata + deltaplus, coord, mydata - deltaless, coord, angle = 90, code = 3, length = 0.02)
	else
		arrows(coord, mydata + deltaplus, coord, mydata - deltaless, angle = 90, code = 3, length = 0.02)
}

grph.createOutputDevice = function(name, width = NULL, height = NULL) {
	if (is.null(name))
		return(NULL)
	endswith = function(name, suffix) { return(substr(name, nchar(name)-nchar(suffix), nchar(name))==paste('.',suffix,sep='')) }

	if (endswith(name, 'pdf')) {
		pdf(file = name, width = width, height = height)
		return('scalable')
	}

	if (endswith(name, 'eps')) {
		postscript(file = name, width = width, height = height, horizontal = FALSE, onefile = FALSE, paper = 'special')
		return('scalable')
	}

	if (endswith(name, 'bmp')) {
		bmp(filename = name, width = width, height = height, units = 'px')
		return('bitmap')
	}

	if (endswith(name, 'jpeg') | endswith(name, 'jpg')) {
		jpeg(filename = name, width = width, height = height, units = 'px', quality = 80)
		return('bitmap')
	}

	if (endswith(name, 'png')) {
		png(filename = name, width = width, height = height, units = 'px')
		return('bitmap')
	}

	if (endswith(name, 'tif') | endswith(name, 'tiff')) {
		tiff(filename = name, width = width, height = height, units = 'px', compression = 'zip')
		return('bitmap')
	}
	return(NULL)
}

#> -------------------------------------------------------------------------------------------------------------------------
#> -------------------------------------------------------------------------------------------------------------------------
grph.drawAxes = function(sides, cex=1.0) {
	for (i in sides) {
		axis(i, at=0:5/5, labels=paste(0:5*20, '%', sep=''), cex.axis=cex)
	}
}

#> -------------------------------------------------------------------------------------------------------------------------
#> grph.drawGrid(horizontal = TRUE, vertical = TRUE, diagonal = TRUE, xlim = c(0,1,.2), ylim = c(0,1,.2)
#>
#> Draws a grid within a plot.
#> horizontal: TRUE if you want horizontal lines drawn.
#> vertical: TRUE if you want vertical lines drawn.
#> diagonal: TRUE if you want a diagonal line (from left-bottom to right-top) drawn.
#> xlim: a set of three values, that could be fed to seq() to determine the anchor points for vertical lines.
#> ylim: a set of three values, that could be fed to seq() to determine the anchor points for horizontal lines.
#> -------------------------------------------------------------------------------------------------------------------------
grph.drawGrid = function(horizontal=TRUE, vertical=TRUE, diagonal=FALSE,
						 xlim=c(0,1,0.1), ylim=c(0,1,0.1)) {
	if (vertical) {
		for (i in seq(xlim[1],xlim[2],xlim[3])) {
			lines(c(i,i), c(ylim[1], ylim[2]), lty=3, col=clio.color('gridcol'))
		}
	}

	if (horizontal) {
		for (i in seq(ylim[1],ylim[2],ylim[3])) {
			lines(c(xlim[1],xlim[2]), c(i,i), lty=3, col=clio.color('gridcol'))
		}
	}

	if (diagonal) {
		lines(c(0,xlim[2]), c(0,ylim[2]), lty=3, col=clio.color('gridcol'))
	}
}

#> -------------------------------------------------------------------------------------------------------------------------
#> grph.drawText(thisdata, yc, thesetexts, cex=.8, cutoff=.05, voffset=0, within=TRUE)
#>
#> Draws text within/over/next to each bar segment in a plot.
#>
#> thisdata: the actual data, as graphed by the plot.
#> yc: the Y coordinates, as returned by the plot() function.
#> thesetexts: the texts to be put within each segment bar.
#> cex: font size.
#> cutoff: the proportion under which a label will not be drawn.
#> voffset: the vertical offset you want to give to the labels. Positive offsets
#>	move the labels upwards, negative offsets do the opposite.
#> within: if TRUE, puts the text within the bar segments; otherwise, it will
#>	put them next to the right of each bar.
#> -------------------------------------------------------------------------------------------------------------------------
grph.drawText = function(thisdata, yc, thesetexts, cex=.8, cutoff=.05, voffset=0, within=TRUE) {
	if (class(thisdata)!='matrix') {
		thisdata = matrix(ncol=1, data=thisdata)
	}

	if (class(thesetexts)!='matrix') {
		thesetexts = matrix(ncol=1, data=thesetexts)
	}

	#> If the texts go within the segment bars, the X position is half way the bar length.
	#> Otherwise, it is equal to the length of bar segments, plus a small horizontal offset.
	if (within) {
		suple = thisdata/2
	} else {
		suple = thisdata + round(max(thisdata)*.01,digits=1)
	}

	#> If there is more than one column, we adjust the horizontal positions.
	if (ncol(thisdata)>1) {
		for (i in 2:ncol(suple)) {
			for (j in 1:(i-1)) {
				suple[,i] = suple[,i] + thisdata[,j]
			}
		}
	}

	#> We remove the labels that do not have enough room to show.
	for (i in 1:nrow(thisdata)) {
		for (j in 1:ncol(thisdata)) {
			if (thisdata[i,j]<cutoff) {
				thesetexts[i,j] = ''
			}
		}
	}

	#> Finally, we put the texts.
	text(suple, yc+voffset, thesetexts, xpd=TRUE, cex=cex, adj=c(if (within) .5 else 0,.5))
}


#> -------------------------------------------------------------------------------------------------------------------------
#> grph.drawProportionWithin(thisdata, yc, digits=1, cutoff=.05, cex=.8)
#>
#> Draws a proportion within each segment bar in a plot.
#>
#> thisdata: the actual data, as graphed by the plot.
#> yc: the Y coordinates, as returned by the plot() function.
#> digits: the number of decimal digits to be fed to cf.asPercentage().
#> cex: font size.
#> cutoff: the proportion under which a label will not be drawn.
#> -------------------------------------------------------------------------------------------------------------------------
grph.drawProportionWithin = function(thisdata, yc, digits=1, cutoff=.05, cex=.9) {
	grph.drawText(thisdata, yc, cf.asPercentage(thisdata, digits=digits), cutoff=cutoff, cex=cex)
}

#> -------------------------------------------------------------------------------------------------------------------------
#> grph.proportionBarplot(data, filename = NULL, thiscolor = NULL,
#>		invert = FALSE, useconfidenceintervals = FALSE, useratiowithin = TRUE, horizontal = TRUE,
#>		width = 4, height = 3, alpha = 0.05, digits = 1, cutoff=.1,
#>		mar = c(1.0,1.0,1.0,1.0), mgp = c(1.0, 0.4, 0.0))
#>
#> Draws a simple proportion barplot.
#>
#> data has to be an Nx1 or Nx2 matrix. If Nx1, this function expect values between 0 and 1. In this case,
#>		useconfidenceintervals cannot be TRUE (if so, it will issue a warning). If Nx2, then [,1]
#>		will be interpreted as numerator and [,2] as denominator of a ratio.
#> thiscolor is a list of colors. length(thiscolor) should be equal to nrow(data), otherwise a warning
#>		will be shown and colors will be reused.
#> invert indicates whether you want to invert the ordering of the rows in data (so it does not appear
#>		from bottom to top).
#> useconfidenceintervals signals that you want to draw confidence intervals on top of each bar.
#> useratiowithin signals that you want to include the ratio within the bar. If data has one column, it
#>		draws the percentage; otherwise, it draws the ratio (that is, 'X/Y')
#> horizontal signals that you want horizontal bars. Horizontal bars are better when you have long labels for each bar.
#> alpha is the usual alpha value, needed if you want confidence intervals.
#> digits is the number of decimal digits in case you want the percentage included within each bar.
#> cutoff is the proportion under which the label will no be drawn. Sometimes some bars are too small to contain a label.
#> -------------------------------------------------------------------------------------------------------------------------
grph.proportionBarplot = function(mydata, filename = NULL, thiscolor = NULL, invert = FALSE,
					    useconfidenceintervals = FALSE, useratiowithin = TRUE, horizontal = TRUE,
					    width = 4, height = 3, alpha = 0.05, digits = 1, cutoff=.1,
					    mar = c(1.0,1.0,1.0,1.0), mgp = c(1.0, 0.4, 0.0), main='') {
	#> Preconditions
	if (!is.numeric(mydata))
		stop('grph.proportionBarplot(arg1) expects arg1 to be numeric. It is not.')
	if (class(mydata)!='matrix' | ncol(mydata)==0 | ncol(mydata)>2)
		stop(paste('grph.ProportionBarplot(arg1) expects arg1[N,1] or arg1[N,2]: arg1 has [', nrow(mydata), ',', ncol(mydata), ']', sep=''))
	if (ncol(mydata)==1 & useconfidenceintervals) {
		warning('grph.ProportionBarplot(useconfidenceintervals=TRUE): Cannot create confidence intervals without knowing the totals.', immediate.=TRUE)
		useconfidenceintervals = FALSE
	}

	#> We copy the data we'll send to graph
	thisdata = if (ncol(mydata)==1) mydata[,1] else mydata[,1]/mydata[,2]
	usingratio = (ncol(mydata)==1)

	#> Preparation of arguments
	if (is.null(thiscolor))
		thiscolor = rainbow(length(thisdata))
	if (length(thiscolor)!=length(thisdata))
		warning('grph.proportionBarplot: number of colors != length(data)')
	thisspace = rep.int(0.2, nrow(mydata))

	#> We invert the data if needed
	if (invert) {
		thisdata = rev(thisdata)
		thiscolor = rev(thiscolor)
		mydata = mydata[nrow(mydata):1,]
	}

	#> We create the device and the main barplot within it.
	tmp = grph.createOutputDevice(filename, width = width, height = height)
	par(mar = mar, mgp = mgp, xpd=T)

	#> We create the barplot
	yc = barplot(
		thisdata,
		xlim = if (horizontal) c(0,1) else NULL,
		ylim = if (horizontal) NULL else c(0,1),
		names.arg = rownames(thisdata),
		col = thiscolor,
		space = thisspace,
		horiz = horizontal,
		las = 1,
		axes = FALSE
	)

	if (main!='') {
		title(main, line=2, cex.main=.9)
	}

	#> Drawing the axes
	if (horizontal) {
		grph.drawAxes(c(1,3))
	} else {
		grph.drawAxes(c(2,4))
	}

	#> We draw the confidence intervals (which in reality are arrows)
	if (useconfidenceintervals)
	{
		grph.drawConfIntervals(thisdata, yc, grph.getBinomialCIRadius(mydata, alpha = alpha), horizontal = horizontal)
	}

	#> We put the labels within the bars
	if (useratiowithin)
	{
		if (usingratio) {
			grph.drawProportionWithin(thisdata, yc, digits=digits, cutoff=cutoff)
		} else {
			grph.drawText(thisdata, yc, paste(mydata[,1], mydata[,2], sep='/'), cutoff=cutoff)
		}
	}

	#> Turn off the device
	if (!is.null(filename)) dev.off()
}

grph.simpleBarplot = function(mydata, xlim = NULL,
							  stacked = FALSE, invert = FALSE, usenumberwithin = TRUE, cutoff = 0, legendposition = NULL,
							  thiscolor = NULL, width = 4, height = 3, mar = c(1.0, 1.0, 1.0, 1.0), mgp = c(1.0, 0.4, 0.0), fontfactor = 0.8,
							  main = '', filename = NULL) {

	#> Preconditions
	if (!is.numeric(mydata) | class(mydata)!='matrix')
		stop('grph.stackedProportionBarplot(arg1) expects arg1 to be a matrix of numeric values. It is not.')
	if (is.null(thiscolor))
		thiscolor = rainbow(ncol(mydata))
	if (length(thiscolor)!=ncol(mydata))
		warning(paste('grph.stackedProportionBarplot(arg1, ..., thiscolor=arg2) expects length(arg2)==ncol(arg1); it is not.'))

	#> We manipulate the data a little bit.
	thisdata = mydata
	if (stacked) {
		for (i in ncol(thisdata):2)
			thisdata[,i] = thisdata[,i] - thisdata[,i-1]
	}

	#> We invert the data if asked to
	if (invert) {
		thisdata = thisdata[nrow(thisdata):1,]
	}

	#> We draw the graph
	grph.createOutputDevice(filename, width = width, height = height)
	par(mar = mar, mgp = mgp, xpd = TRUE)
	yc = barplot(t(thisdata),
				 xlim = xlim,
				 horiz = TRUE,
				 width = 0.2,
				 las = 1,
				 cex.names = fontfactor,
				 col = thiscolor,
				 axes = TRUE,
				 main = main,
				 cex.main = fontfactor
	)

	#> We put the numbers
	if (usenumberwithin) {
		grph.drawText(thisdata, yc, thisdata, cutoff = cutoff, cex = thisfontfactor)
	}

	#> Legend
	if (!is.null(legendposition))
		legend(legendposition, colnames(thisdata), fill = thiscolor, cex = fontfactor, xpd = TRUE)

	#> We finish.
	if (!is.null(filename)) dev.off()
}


#> -------------------------------------------------------------------------------------------------------------------------
#> grph.stackedProportionBarplot(data, filename = NULL, thiscolor = NULL,
#>		useratiowithin = FALSE, uselegend = TRUE, invert = FALSE, stacked = FALSE, horizontal = TRUE,
#>		width = 4, height = 3, digits = 0,
#>		mar = c(1.0,1.0,1.0,1.0), mgp = c(1.0, 0.4, 0.0), legendoffset = 0.2)
#>
#> Draws a stacked proportion barplot; that is, M bars where each bar has N categories that do not add up to 100%. The graph
#> may include the percentages within each bar segment, and by default includes a legend. This type of graph with stacked=TRUE
#> is confusing, as it presents several bars on top of each other. Use with moderation :)
#>
#> data has to be a matrix of MxN, where 0<matrix[i,j]<1 for all i,j.
#> thiscolor is a list of colors. length(thiscolor) should be equal to nrow(data), otherwise a warning will be shown and colors
#>		will be reused.
#> useratiowithin signals that you want to include the ratio within the bar.
#> horizontal signals that you want horizontal bars. Horizontal bars are better when you have long labels for each bar.
#> -------------------------------------------------------------------------------------------------------------------------
grph.stackedProportionBarplot = function(mydata, filename = NULL, thiscolor = NULL,
						     useratiowithin = FALSE, uselegend = TRUE, invert = FALSE, stacked = FALSE, horizontal = TRUE,
						     width = 4, height = 3, digits = 0, cutoff = .1,
						     mar = c(1.0,1.0,1.0,1.0), mgp = c(1.0, 0.4, 0.0), legendoffset = 0.2, main = '', cex=.9) {

	#> Preconditions
	if (!is.numeric(mydata) | class(mydata)!='matrix')
		stop('grph.stackedProportionBarplot(arg1) expects arg1 to be a matrix of numeric values. It is not.')
	if (is.null(thiscolor))
		thiscolor = rainbow(ncol(mydata))
	if (length(thiscolor)!=ncol(mydata))
		warning(paste('grph.stackedProportionBarplot(arg1, ..., thiscolor=arg2) expects length(arg2)==ncol(arg1); it is not.'))
	if (ncol(mydata)<2)
		stop('grph.stackedProportionBarplot(arg1): arg1 has only one column. Use grph.proportionBarplot() instead.')

	#> We manipulate the data a little bit.
	thisdata = mydata
	if (stacked) {
		for (i in ncol(thisdata):2)
			thisdata[,i] = thisdata[,i] - thisdata[,i-1]
	}

	#> We invert the data if asked to
	if (invert) {
		thisdata = thisdata[nrow(thisdata):1,ncol(thisdata):1]
		mydata = mydata[nrow(mydata):1,ncol(thisdata):1]
	}

	#> We draw the graph
	tmp = grph.createOutputDevice(filename, width = width, height = height)
	par(mar = mar, mgp = mgp, xpd = TRUE)
	yc = barplot(t(thisdata),
			 horiz = horizontal,
			 beside = !stacked,
			 width = 0.2,
			 las = 1,
			 cex.names = 0.8,
			 col = if (invert) rev(thiscolor) else thiscolor,
			 axes = FALSE,
			 main = main
	)

	#> Ratio within each bar segment
	if (useratiowithin) {
		if (stacked) {
			grph.drawProportionWithin(t(thisdata), yc, digits=digits, cutoff=cutoff, horizontal=horizontal, cex=cex)
		} else {
			suple = as.numeric(t(thisdata))/2
			mylabs = paste(round(100*t(thisdata), 0), '%', sep='')
			text(if (horizontal) suple else yc, if (horizontal) yc else suple, mylabs,
				 xpd = TRUE,
				 adj = if (horizontal) c(0.5, NA) else c(NA, 0.5))
		}
	}

	#> Legend
	if (uselegend & !is.null(colnames(thisdata)))
		legend('bottom', if (invert) rev(colnames(thisdata)) else colnames(thisdata),
			 inset = -1*legendoffset,
			 fill = thiscolor,
			 cex = 0.7,
			 xpd = TRUE)

	#> Axes
	if (horizontal) {
		grph.drawAxes(c(1,3))
	} else {
		grph.drawAxes(c(2,4))
	}

	if (!is.null(filename)) dev.off()
}

#> -------------------------------------------------------------------------------------------------------------------------
#> grph.stackedFullProportionBarplot(data, filename = NULL, thiscolor = NULL,
#>		useratiowithin = FALSE, uselegend = TRUE, invert = FALSE, showaxes = TRUE, horizontal = TRUE,
#>		width = 4, height = 3, digits = 0, cutoff = .1,
#>		mar = c(1.0,1.0,1.0,1.0), mgp = c(1.0, 0.4, 0.0), legendoffset = 0.2)
#>
#> Draws a full stacked proportion barplot; that is, M bars where each bar has N categories that all add up to 100%. The graph
#> may include the percentages within each bar segment, and by default includes a legend.
#>
#> data has to be a matrix of MxN. If the sum of the values of the first row add up to 1.0 (with a .001 tolerance),
#> then each value will be interpreted as proportions. Otherwise, rows will be summed up and divided by the total.
#> showaxes signals whether you want the axes drawn or not.
#> The rest of the parameters are similar to the previous functions.
#> -------------------------------------------------------------------------------------------------------------------------
grph.stackedFullProportionBarplot = function(mydata, filename = NULL, thiscolor = NULL,
						     useratiowithin = FALSE, uselegend = TRUE, invert = FALSE,
						     showaxes = TRUE, horizontal = TRUE,
						     width = 4, height = 3, digits = 0, cutoff = .1, cex=c(1,1,1),
						     mar = c(1.0,1.0,1.0,1.0), mgp = c(1.0, 0.4, 0.0), legendoffset = 0.2, main = '') {
	#> Preconditions
	if (is.null(thiscolor))
		thiscolor = rainbow(ncol(mydata))
	if (length(thiscolor)!=ncol(mydata))
		warning(paste('grph.stackedFullProportionBarplot(arg1, ..., thiscolor=arg2) expects length(arg2)==ncol(arg1); it is not.'))

	#> We manipulate the data a little bit.
	if (abs(sum(mydata[1,])-1.0)<=0.001) {
		thisdata = mydata
	} else {
		thisdata = mydata / rowSums(mydata)
	}

	#> We invert the data if asked to
	if (invert) thisdata = thisdata[nrow(thisdata):1,]

	#> We draw the graph
	tmp = grph.createOutputDevice(filename, width = width, height = height)
	par(mar = mar, mgp = mgp, xpd = TRUE)
	yc = barplot(t(thisdata),
			 horiz = horizontal,
			 width = 0.2,
			 las = 1,
			 col = thiscolor,
			 axes = FALSE,
			 cex.names = cex[1]
	)

	#> Axes
	if (showaxes) {
		if (horizontal) {
			grph.drawAxes(c(1,3))
		} else {
			grph.drawAxes(c(2,4))
		}
	}

  #> Title
	if (main!='') {
	  title(main=main, line=2.2, cex.main=0.9)
	}

	#> Ratio within each bar segment
	if (useratiowithin) {
		grph.drawProportionWithin(thisdata, yc, digits=digits, cutoff=cutoff, cex=cex[2])
	}

	#> Legend
	if (uselegend & !is.null(colnames(thisdata)))
		legend('right', colnames(thisdata), inset = -1*legendoffset, fill = thiscolor, cex = cex[3], xpd = TRUE)

	if (!is.null(filename)) dev.off()
}
