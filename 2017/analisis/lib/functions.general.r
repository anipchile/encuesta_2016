#> ---------------------------------------------------------------------------
#> Basic configuration. These variables and settings are supposed to be
#> available throughout the system.
#> ---------------------------------------------------------------------------
options(stringsAsFactors = FALSE)
options(scipen=1)

#> ---------------------------------------------------------------------------
#> Miscelaneous functions
#> ---------------------------------------------------------------------------
expandfile = function(file='', path='data') {
	if (path=='data')
		return(paste(clio.root, 'data/', file, sep=''))
	if (path=='output')
		return(paste(clio.root, 'output/', file, sep=''))
	if (path=='figure')
		return(paste(clio.root, 'output/figure/', file, sep=''))
	if (path=='r' | path=='R')
		return(paste(clio.root, 'R/', file, sep=''))
	if (length(grep("\\/$", path))==0)
		path = paste(path,'/',sep='')
	return(paste(clio.root, path, file, sep=''))
}

# runandlog = function(com) {
# 	tmp = system(com, intern=TRUE)
# 	if (length(tmp)>0)
# 		for (i in tmp)
# 			ms.addLog('[', com, '] ', i)
# }

ms.writeFileHeader = function(filename, format='tex') {
	if (format=='tex') {
		cat(paste("%> File", filename, "created automatically on", date(), "\n\n"), file = filename)
	} else if (format=='html') {
		cat(paste("<!-- File", filename, "created automatically on", date(), "-->\n"), file=filename)
	} else if (format=='raw') {
		cat(paste("#> File", filename, "created automatically on", date(), "\n"), file=filename)
	} else {
		warning("ms.writeFileHeader: format not supported, no output.")
	}
}

ms.read.csv = function(filename, path = expandfile(path='data')) {
	return(read.csv(
		paste(path, filename, sep=''),
		header=TRUE,
		sep=",",
		row.names=NULL,
		quote = "'\"",
		na.strings=c("", "\"\""),
		stringsAsFactors=FALSE
	))
}

ms.write.csv = function(thisdata, filename, path = expandfile(path='output')) {
	if (substr(path,nchar(path),nchar(path))!='/')
		path = paste(path, '/', sep='')
	if (substr(filename,nchar(filename)-3,nchar(filename))!='.csv')
		filename = paste(filename, '.csv', sep='')
	write.csv(thisdata, paste(path, filename, sep=''), row.names=FALSE)
}

ms.spit = function(..., file = NULL, head = "") {
	if (is.null(file)) stop('File cannot be null.')
	cat(head, as.character(...), "\n", file = file, sep = "", append = TRUE)
}

ms.startLogFile = function() {
	ms.writeFileHeader(log.file, format='raw')
}

ms.addLog = function(...) {
	ms.spit(as.character(date()), ': ', ..., file = log.file, head='#> ')
}

ms.addVarVal = function(var, val, comment = NULL) {
	if (is.null(comment)) {
		ms.spit(as.character(var), ': ', as.character(val), file = log.file, head = '')
	} else {
		ms.spit(as.character(var), ': ', as.character(val), ' #> ', comment, file = log.file, head = '')
	}
}

#> ---------------------------------------------------------------------------
#> Formatting functions
#> ---------------------------------------------------------------------------
cf.asPercentage <- function(val, digits=1, frequency=FALSE, keepfrequency=FALSE) {
	cf.asPercent <- function(x) {
		return(paste(round(100*as.numeric(x),digits), '%', sep=''))
	}

	if (class(val)=='numeric' | class(val)=='character') {
		tmp <- cf.asPercent(val)
		return(replace(tmp, which(tmp=='NA%' | tmp=='Inf%'), ''))
	} else if (class(val)=='matrix' | class(val)=='table') {
		if (frequency) {
			val2 <- val/sum(val)
		} else {
			val2 <- val
		}

		if (!keepfrequency) {
			return(apply(val2, c(1,2), cf.asPercent))
		} else {
			return(matrix(paste(val, ' (', apply(val2, c(1,2), cf.asPercent), ')', sep=''), nrow=nrow(val), dimnames=dimnames(val)))
		}
	} else {
		stop(paste('cf.asPercentage(val): I don\'t know how to convert val, because class(val)==', class(val), sep=''))
	}
}

cf.asSingleGrade = function(x) {
	if (is.character(x))
		stop('cf.asSingleGrade(x): x must be numeric.')

	if (is.na(x) | is.infinite(x)) { return('') }

	tp1 = round(x*6+1, digits=1)
	tp0 = round(x*6+1, digits=0)
	if (tp0-tp1==0)
		return(paste(as.character(tp1), '0', sep='.'))
	else
		return(as.character(tp1))
}

cf.asGrade = function(val) {
	if (class(val)=='numeric' | class(val)=='character') {
		return(sapply(val, cf.asSingleGrade))
	} else if (class(val)=='matrix' | class(val)=='table') {
		return(apply(val, c(1,2), cf.asSingleGrade))
	} else {
		stop(paste('cf.asGrade(val): I don\'t know how to convert val, because class(val)==',
				   class(val), sep=''))
	}
}

cf.asPValue = function(val, digits=4, latex=FALSE) {
	tp = round(as.numeric(val), digits)
	if (tp==0)
		fin = paste("<0.",paste(rep.int('0',digits-1), sep='', collapse=''), "1", sep='')
	else
		fin = paste("=",tp, sep='')

	if (latex)
		fin = paste('$', fin, '$', sep='')

	return(fin)
}

cf.asHTMLTable = function(mat, usenames = TRUE, usefirstcol = FALSE, usefirstrow = FALSE, usetopleft = FALSE) {
	mat = apply(mat, c(1,2), function(x) { if (is.na(x)) {''} else {x}})
	mat2 = mat3 = matrix(ncol=ncol(mat), nrow=nrow(mat), data = 'td')

	if (usefirstrow) {
		mat2[1,] = "th class='t'"
		mat3[1,] = "th"
	}
	if (usefirstcol) {
		mat2[,1] = "th class='l'"
		mat3[,1] = "th"
	}
	if (usefirstrow & usefirstcol) {
		mat2[1,1] = if (usetopleft) "th class='t l'" else 'td'
		mat3[1,1] = if (usetopleft) 'th' else 'td'
	}

	if (usenames) {
		if (!is.null(colnames(mat))) {
			mat = rbind(colnames(mat), mat)
			mat2 = rbind(rep.int("th class='t'", ncol(mat2)), mat2)
			mat3 = rbind(rep.int("th", ncol(mat3)), mat3)
		}
		if (!is.null(rownames(mat))) {
			mat = cbind(rownames(mat), mat)
			mat2 = cbind(rep.int("th class='l'", nrow(mat2)), mat2)
			mat3 = cbind(rep.int("th", nrow(mat3)), mat3)
		}
		if (!is.null(colnames(mat)) & !is.null(rownames(mat))) {
			mat2[1,1] = if (usetopleft) "th class='t l'" else 'td'
			mat3[1,1] = if (usetopleft) 'th' else 'td'
		}
	}

	tmp = matrix(ncol=ncol(mat), data = paste('<', mat2, '>', mat, '</', mat3, '>', sep=''))
	cal = NULL
	for (i in 1:nrow(tmp))
		cal = c(cal, '<tr>', paste(tmp[i,], sep='', collapse=''), '</tr>\n')
	return(paste(cal, collapse=''))
}

cf.secAsMinSec = function(seconds, short=FALSE) {
	min = trunc(seconds/60)
	sec = trunc(seconds - min*60)
	presec = rep.int('', length(sec))
	presec[sec<10] = '0'
	return(if (short) paste(min,'min', sec, 'sec') else paste(min,':',presec,sec,sep=''))
}

cf.msAsSec = function(milliseconds, label = TRUE) {
  if (label)
    return(paste(round(milliseconds/1000,1),'sec'))
  else
    return(round(milliseconds/1000,1))
}

#> ---------------------------------------------------------------------------
#> String functions
#> ---------------------------------------------------------------------------
str.trim = function(x, lower=FALSE) {
	x <- gsub("\\s+", " ", x)
	x <- gsub("^\\s+", "", x)
	x <- gsub("\\s+$", "", x)
	if (lower) { x <- tolower(x) }
	return(x)
}

str.asId = function(x, stripspaces=TRUE, replacespaces=TRUE, stripquotes=TRUE, strippunct=TRUE) {
	x = tolower(str.trim(x))
	x = gsub("[Áá]", "a", x)
	x = gsub("[Éé]", "e", x)
	x = gsub("[Íí]", "i", x)
	x = gsub("[Óó]", "o", x)
	x = gsub("[Úú]", "u", x)
	x = gsub("[Ññ]", "n", x)

	if (strippunct)  { x = gsub("[,\\.\"!@#\\$%\\^&\\*\\(\\)\\-_=\\+]", "", x) }
	if (stripquotes) { x = gsub("['\"]", "", x) }
	if (stripspaces)
	{
		x = gsub("\\s+", "", x)
	} else {
		if (replacespaces) {
			x = gsub("\\s+", "_", x)
		} else {
			x = gsub("\\s+", " ", x)
		}
	}
	return(x)
}

str.asTitle = function(x) {
	return(gsub("(^|\\s)(\\w)", "\\1\\U\\2", x, perl=TRUE))
}

str.fixLatin1Encoding = function(x) {
	x = gsub("\xe1", "á", x)
	x = gsub("\xe8", "é", x)
	x = gsub("\xe9", "é", x)
	x = gsub("\xec", "í", x)
	x = gsub("\xed", "í", x)
	x = gsub("\xf3", "ó", x)
	x = gsub("\xfa", "ú", x)
	x = gsub("\xfc", "u", x) # u con dieresis
	x = gsub("\xf1", "ñ", x)
	x = gsub("\xc1", "Á", x)
	#x = gsub("", "É", x)
	x = gsub("\xcd", "Í", x)
	x = gsub("\xd3", "Ó", x)
	#x = gsub("", "Ú", x)
	#x = gsub("", "Ñ", x)
	return(x)
}

str.shorten <- function(x, len=60, addellipsis=TRUE) {
  x <- substr(str.trim(x), 1, len)
  return(sapply(x, function(y) { return(paste(y, if (nchar(y)>=len) '(...)' else '', sep='')) }))
}
