#-------------------------------------------------------------------------------
#
#           writeDatafileR
#
#-------------------------------------------------------------------------------

#' @title Writes from an R list object containing data to a form compatable with WinBUGS. 
#'
#' @description Writes from R to file "towhere" text defining a list containing "DATA" in a form compatable with WinBUGS.
#'
#' @details 
#' The function performs considerable checking of DATA argument. Since WinBUGS requires numeric input, no factors or character vectors are allowed. All data must be named, either as named elements of DATA (if it is a list) or else using the names given in data frames. Data frames may contain matrices. Arrays of any dimension are rearranged to be in row-major order, as required by WinBUGS. Scientific notation is also handled properly. In particular, the number will consist of a mantissa _containing a decimal point_ followed by "E", then either "+" or "-", and finally a _two-digit_ number.
#'
#' @param DATA either a data frame or else a list consisting of any combination of scalars, vectors, arrays or data frames (but not lists). If a list, all list elements that are not data.frames must be named. Names of data.frames in DATA are ignored.
#' @param towhere file to receive output. Is "toWinBUGS.txt" by default.
#' @param fill If numeric, number of columns for output. (Default is 80.) If FALSE, output will be on one line. If TRUE, number of columns is given by .Options$width.
#'
#' @return Text defining a list is output to file "towhere".
#'

#'
#' @author Terry Elrod
#' @rdname writeDatafileR
#' @export writeDatafileR 


"writeDatafileR" <-
		function(DATA, towhere = "toWinBUGS.txt", fill = 80)
{
#
# Writes from R to file "towhere" text defining a list containing "DATA" in a form compatable with WinBUGS.
# Required arguments:
# DATA - either a data frame or else a list consisting of any combination of scalars, vectors, arrays or data frames (but not lists).
#   If a list, all list elements that are not data.frames must be named. Names of data.frames in DATA are ignored.
# Optional arguments:
# towhere - file to receive output. Is "toWinBUGS.txt" by default.
# fill - If numeric, number of columns for output. (Default is 80.) If FALSE, output will be on one line. If TRUE, number of
#   columns is given by .Options$width.
# Value:
# Text defining a list is output to file "towhere". 
# Details:
#  The function performs considerable checking of DATA argument. Since WinBUGS requires numeric input, no factors or character vectors
# are allowed. All data must be named, either as named elements of DATA (if it is a list) or else using the names given in data frames.
# Data frames may contain matrices. 
# Arrays of any dimension are rearranged to be in row-major order, as required by WinBUGS. Scientific notation is also handled properly.
# In particular, the number will consist of a mantissa _containing a decimal point_ followed by "E", then either "+" or "-", and finally 
# a _two-digit_ number. 
# Written by Terry Elrod. Disclaimer: This function is used at the user's own risk. 
# Please send comments to Terry.Elrod@UAlberta.ca.
# Revision history: 2003-11-14: Fixed to handle missing values properly. (Thanks to Kjetil Halvorsen.)
#					2003-11-14:	Tests for valid Winbugs names. Forces single precision for all numbers.
	formatDataR <- 
			#
			# Prepared DATA for input to WinBUGS.
			function(DATA)
	{
		testWinbugsNames <-
				#
				# Checks to see that all names are valid...
				function(na){
			baseTestString <- c(
					"The following variable names are invalid in R: ",
					"The following variable names are used more than once: ",
					"The following variable names have more than 8 characters: ",
					"The following variable names contain two or more periods in a row: ",
					"The following variable names end in a period: ")
			# Testing for invalid R names ...
			nameTest1 <- make.names(na, unique = FALSE)
			nameTest1 <- (nameTest1 != na)
			# Testing for duplicate names....
			nameTest2 <- make.names(na, unique = TRUE)
			nameTest2 <- (nameTest2 != na)
			# Testing for excess length...
			nameTest3 <- substring(na, 1, 8)
			nameTest3 <- (na != nameTest3)
			# Testing for presence of two or more successive periods ...
			nameTest4 <- regexpr("\\.\\.", na)
			nameTest4 <- (nameTest4 > 0)
			# Testing for presence of ending period ...
			nameTest5 <- regexpr("\\.$", na)
			nameTest5 <- (nameTest5 > 0)
			# Assembling tests and reporting results...
			nameTest <- cbind(nameTest1, nameTest2, nameTest3, nameTest4, nameTest5)
			if(any(nameTest)){
				nameTestInd <- apply(nameTest, 2, any)
				whichTest <- seq(along=nameTestInd)[nameTestInd]
				testString <- "There were problems with names of one or more variables:"
				if(nameTestInd[1])
					testString <- paste(testString, paste(baseTestString[1], paste(na[nameTest[,1]], collapse = ", "), sep=""), sep="\n")
				if(nameTestInd[2])
					testString <- paste(testString, paste(baseTestString[2], paste(unique(na[nameTest[,2]]), collapse = ", "), sep=""), sep="\n")
				if(nameTestInd[3])
					testString <- paste(testString, paste(baseTestString[3], paste(na[nameTest[,3]], collapse = ", "), sep="") ,sep="\n")
				if(nameTestInd[4])
					testString <- paste(testString, paste(baseTestString[4], paste(na[nameTest[,4]], collapse = ", "), sep="") ,sep="\n")
				if(nameTestInd[5])
					testString <- paste(testString, paste(baseTestString[5], paste(na[nameTest[,5]], collapse = ", "), sep="") ,sep="\n")
				stop(testString)
			}
			invisible(0)
		}
		toSingle <- 
				#
				# Takes numeric vector, adds period to mantissa in scientific notation (if necessary),
				#	converts "e" to "E", expresses mantissa with at most 10 characters,
				#	and eliminates trailing zeros from mantissa.
				function(x)
		{
			myRegMatchPos <- 
					#
					# Duplicates regMatchPos in the S4 engine...
					function(w, txt)
			{
				st <- regexpr(txt, w)
				pplusind <- (st > 0)
				fin <- st + attr(st, "match.length") - 1
				pplus <- cbind(st, fin)
				pplus[!pplusind,  ] <- NA
				pplus
			}
			xdim <- dim(x)
			x <- as.single(x)
			x <- sapply(x,function(y) format(y, digits=7, trim=TRUE))
			# First to look for positives:
			pplus <- myRegMatchPos(x, "e\\+0")
			pplusind <- apply(pplus, 1, function(y)
						(!any(is.na(y))))
			if(any(pplusind)) {
				# Making sure that periods are in mantissa...
				init <- substring(x[pplusind], 1, pplus[
								pplusind, 1] - 1)
				#...preceeding exponent
				pper <- myRegMatchPos(init, "\\.")
				pperind <- apply(pper, 1, function(y)
							(all(is.na(y))))
				if(any(pperind))
					init[pperind] <- paste(init[pperind],
							".0", sep = "")
				# Changing the format of the exponent...
				x[pplusind] <- paste(init, "E+", substring(
								x[pplusind], pplus[pplusind, 2] + 1),
						sep = "")
			}
			# Then to look for negatives:
			pminus <- myRegMatchPos(x, "e\\-0")
			pminusind <- apply(pminus, 1, function(y)
						(!any(is.na(y))))
			if(any(pminusind)) {
				# Making sure that periods are in mantissa...
				init <- substring(x[pminusind], 1, pminus[
								pminusind, 1] - 1)
				#...preceeding exponent
				pper <- myRegMatchPos(init, "\\.")
				pperind <- apply(pper, 1, function(y)
							(all(is.na(y))))
				if(any(pperind))
					init[pperind] <- paste(init[pperind],
							".0", sep = "")
				# Changing the format of the exponent...
				x[pminusind] <- paste(init, "E-", substring(
								x[pminusind], pminus[pminusind, 2] +
										1), sep = "")
			}
			x
		}
		if(!is.list(DATA))
			stop("DATA must be a named list or data frame.")
		dlnames <- names(DATA)
		if(is.data.frame(DATA))
			DATA <- as.list(DATA)
		#
		# Checking for lists in DATA....
		lind <- sapply(DATA, is.list)
		# Checking for data frames in DATA....
		dfind <- sapply(DATA, is.data.frame)
		# Any lists that are not data frames?...
		if(any(lind & !dfind)) stop("DATA may not contain lists.")
		# Checking for unnamed elements of list that are not data frames....
		if(any(dlnames[!dfind] == "")) stop(
					"When DATA is a list, all its elements that are not data frames must be named."
			)
		if(any(dfind)) {
			dataold <- DATA
			DATA <- vector("list", 0)
			for(i in seq(along = dataold)) {
				if(dfind[i])
					DATA <- c(DATA, as.list(dataold[[i]]))
				else DATA <- c(DATA, dataold[i])
			}
			dataold <- NULL
		}
		dlnames <- names(DATA)
		# Making sure all names are valid ...
		testWinbugsNames(dlnames)
		# Checking for factors....
		factorind <- sapply(DATA, is.factor)
		if(any(factorind))
			stop(paste(
							"DATA may not include factors. One or more factor variables were detected:",
							paste(dlnames[factorind], collapse = ", ")))
		# Checking for character vectors....
		charind <- sapply(DATA, is.character)
		if(any(charind))
			stop(paste(
							"WinBUGS does not handle character data. One or more character variables were detected:",
							paste(dlnames[charind], collapse = ", ")))
		# Checking for complex vectors....
		complexind <- sapply(DATA, is.complex)
		if(any(complexind))
			stop(paste(
							"WinBUGS does not handle complex data. One or more complex variables were detected:",
							paste(dlnames[complexind], collapse = ", ")))
		# Checking for values farther from zero than 1E+38 (which is limit of single precision)....
		toobigind <- sapply(DATA, function(x)
				{
					y <- abs(x[!is.na(x)])
					any(y[y > 0] > 1.0e+038)
				}
		)
		if(any(toobigind))
			stop(paste(
							"WinBUGS works in single precision. The following variables contain data outside the range +/-1.0E+38: ",
							paste(dlnames[toobigind], collapse = ", "),
							".\n", sep = ""))
		# Checking for values in range +/-1.0E-38 (which is limit of single precision)....
		toosmallind <- sapply(DATA, function(x)
				{
					y <- abs(x[!is.na(x)])
					any(y[y > 0] < 1.0e-038)
				}
		)
		n <- length(dlnames)
		data.string <- as.list(rep(NA, n))
		for(i in 1:n) {
			ldi <- length(DATA[[i]])
			if(ldi == 1) {
				ac <- toSingle(DATA[[i]])
				data.string[[i]] <- c(
						names(DATA)[i], 
						"=",
						paste(ac), 
						"," )
				next
			}
			if(is.vector(DATA[[i]]) & ldi > 1) {
				ac <- toSingle(DATA[[i]])
				data.string[[i]] <- c(
						names(DATA)[i],
						"= c(",
						paste(ac[-ldi], ",", sep=""), 
						paste(ac[ldi], ")", sep=""),
						"," )
				next
			}
			if(is.array(DATA[[i]])) {
				ac <- toSingle(aperm(DATA[[i]]))
				data.string[[i]] <- c(
						names(DATA)[i], 
						"= structure(.Data = c(", 
						paste(ac[-ldi], ",", sep=""),
						paste(ac[ldi], "),", sep=""), 
						".Dim=c(",
						paste(as.character(dim(DATA[[i]])),collapse = ", "),
						"))",
						"," )
			}
		}
		data.string <- unlist(data.string)			
		data.tofile <- c(
				"list(", 
				data.string[-length(data.string)], 
				")" )
		if(any(toosmallind))
			warning(paste(
							"WinBUGS works in single precision. The following variables contained nonzero data",
							"\ninside the range +/-1.0E-38 that were set to zero: ",
							paste(dlnames[toosmallind], collapse = ", "),
							".\n", sep = ""))
		return(data.tofile)
	}
	cat(formatDataR(DATA), file = towhere, fill = fill)
	formatDataR(DATA)
	invisible(0)
}
