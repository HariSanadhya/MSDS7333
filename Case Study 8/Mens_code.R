library(stringr)
library(XML)
library(gdata)

extractSingleItem <- function(strLine, pattern){
#' Extract a single item from a list of strings and that too from the beginning given the pattern in from of regular expression
#'
#' \code{extractSingleItem} returns the list of strings with the first match found for the given pattern and the rest of the string
#'
#' @param strLine - A list of String from which the element patching the pattern is to be extracted.
#' @param pattern - A regular expression or the string to be searched in the strings contained in strLine parameter 
#' @return List of lists, i.e. a list of the first matching string from each element of the list strLine and list of the remaining part of string after removing the matched component.
#'
#'
    itemVal <- str_match(strLine, pattern)[1, 1]
    len <- nchar(itemVal)
    
    if (!is.na(itemVal)) {
        loc <- regexpr(itemVal, strLine)
        newStrLine <- substr(strLine, start = loc + len, stop = nchar(strLine))   
    } else {
        newStrLine <- strLine
    }
    
    return (c(itemVal, newStrLine))
}

parseLine <- function(strLine){
#' Parses the lines from the list of string strLine 
#'
#' \code{parseLine} returns a list containing the parsed values of strLine
#'
#' @param strLine - A list of String to be parsed to corresponding column values
#' @return a list containing the parsed values of strLine as place, divTot, num, name, ag, homeTown, gunTime, netTime, pace
#'
#'    
    placeOut <- extractSingleItem(strLine, "\\d+")
    place <- str_trim(placeOut[1])
    newStrLine <- placeOut[2]
    
    divTotOut <- extractSingleItem(newStrLine, "\\d+/\\d+")
    divTot <- str_trim(divTotOut[1])
    newStrLine <- divTotOut[2]
    
    numOut <- extractSingleItem(newStrLine, "\\d+")
    num <- str_trim(numOut[1])
    newStrLine <- numOut[2]
    
    nameOut <- extractSingleItem(newStrLine, "[A-Za-z\\-\\'\\.\\s]+")
    name <- str_trim(nameOut[1])
    newStrLine <- nameOut[2]
    
    agOut <- extractSingleItem(newStrLine, "\\d+")
    ag <- str_trim(agOut[1])
    newStrLine <- agOut[2]
    
    homeTownOut <- extractSingleItem(newStrLine, "([A-Za-z]+[\\s]*)+")
    homeTown <- str_trim(homeTownOut[1])
    newStrLine <- homeTownOut[2]
    
    times <- str_match_all(newStrLine, "\\d+[\\:]{1}\\d+([\\:]{1}\\d+)?[#|*]*")[[1]]
    gunTime <- str_trim(times[1, 1])
    netTime <- str_trim(times[2, 1])
    pace <- str_trim(times[3, 1])
    
    return (c(place, divTot, num, name, ag, homeTown, gunTime, netTime, pace))
    
}

extractResTable <- function(url, year = 1999, men = T, file = NULL, saveFileAsTxt = TRUE) {
#' extract results from given cherry blossom url, find preformatted text and return as chr vector
#'
#' \code{extractResTable} returns character vector 
#'
#' @param url - URL to the Cherry Blossoms results webpage 
#' @param year - Year corresponding to which the url contains the results
#' @param men - T if url has mens results and F if url carries womens results
#' @param file - The file name to be used to save this extracted text
#' @param saveFileAsTxt - TRUE if the data extract is to be saved on the disk otherwise FALSE
    
#' @return a character vector containing the data extracted from the given URL
#'
#'      
    
    # Parse the url data
    doc <- htmlParse(url)
    
    if (year == 2000) {
        #Get text from 4th font element
        #File is ill-formed so <pre> search doesn't work.
        ff <- getNodeSet(doc, "//font")
        txt <- xmlValue(ff[[4]])
        els <- strsplit(txt, "\r\n")[[1]]
    } else if (year == 2009 & men == T) { # Special formatting required for 2009 file for men
        #The html for this is nooooo fun
        dp <- getNodeSet(doc, "//div//pre")
        nodeVals <- lapply(dp, xmlValue)
        
        eqIndex <- grep("^===", nodeVals)
        spacerRow <- nodeVals[eqIndex][[1]]
        vecSpacerRow <- str_split(str_trim(spacerRow), "[\\s|Â]+")[[1]]
        
        #Need to adjust Gun Time and Net Time columns b/c they are too short for some of their values
        vecSpacerRow[7] <- paste(vecSpacerRow[7],"=", sep = "")
        vecSpacerRow[8] <- paste(vecSpacerRow[8],"=", sep = "")
        
        #manually type in headers; I know this isn't ideal, but code is only useful for Men's 2009 results anyway
        vecHeaderRow <- c("Place", "Div/Tot", "Num", "Name", "Ag", "Hometown", "Gun Tim", "Net Tim", "Pace")
        
        bodyNodeVals <- nodeVals[(eqIndex + 1):(length(nodeVals) - 2)]
        
        colLens <- sapply(vecSpacerRow, nchar, USE.NAMES = F)
        bodyMat <- t(sapply(bodyNodeVals, parseLine))
        
        preHeadNodeVals <- nodeVals[(1):(eqIndex - 2)]
        vecPreHeadText <- sapply(preHeadNodeVals, gsub, pattern = "Â", replacement = " ")
        vecPreHeadText <- sapply(vecPreHeadText, str_trim)
        
        footerNodeVals <- nodeVals[(length(nodeVals) - 1):(length(nodeVals))]
        vecFooterText <- sapply(footerNodeVals, gsub, pattern = "Â", replacement = " ")
        #footerMat <- t(sapply(vecFooterText, function(x) c(x, rep(" ", 8)), USE.NAMES = F))
        
        dataMat <- rbind(vecHeaderRow, vecSpacerRow, bodyMat, deparse.level = 0)
        
        #write to Txt file
        lapply(vecPreHeadText, write, "temp2009.txt", append = T)
        write.fwf(dataMat, file = "temp2009.txt", width = colLens, colnames = F, append = T)
        lapply(vecFooterText, write, "temp2009.txt", append = T)
        
        #convert contents of txt file to chr vector
        els <- readLines("temp2009.txt")
        file.remove("temp2009.txt")
    }
    else {
        # Read the data within the pre node of the parsed HTML
        preNode <- getNodeSet(doc, "//pre")
        
        # Retrieve entire text from the given url
        txt <- xmlValue(preNode[[1]])
        
        # Considering new line character as \r\n, split the text into individual lines
        els <- strsplit(txt, "\r\n")[[1]]
        
        # If new line character is not \r\n then check for line character as \n
        if(length(els)==1)
            els = strsplit(txt, "\n")[[1]]
    }
    
    # Code to remove any blank lines that might be present
    els <- els[!trimws(els) %in% ""]
    
    # Code the remove the lines beginning with # and *. These lines are footer lines and does not carry meaningful data 
    els <- els[!startsWith(trimws(els) , '#')]
    els <- els[!startsWith(trimws(els) , '*')]
    
    # Save the individual years men and womens data in txt format if saveFileAsTxt parameter is True
    if(saveFileAsTxt){
        if (men == T) {
            subDir <- "MenTxt"
        } else {
            subDir <- "WomenTxt"
        }

        if (!(is.null(file))) {
            if(!(dir.exists(subDir))) {
                dir.create(subDir)
            } 
            writeLines(els, file.path(subDir, file))
        }
    }
    return(els)
}


findColLocs <- function(spacerRow) {
#' Find the column locations from a header (spacerRow string) record where columns are separated by spaces 
#'
#' \code{findColLocs} returns a list containing the location of spaces in the string contained in parameter spacerRow
#'
#' @param spacerRow - A String from which the location of spaces is to be retrieved.
#' @return a list containing the location of spaces in the string contained in parameter spacerRow. The first element of the list returned is always 0 and the last element is length of the spacerRow string plus one if the last character is not blank.
#'
#'
    spaceLocs <- gregexpr(" ", spacerRow)[[1]]
    rowLength <- nchar(spacerRow)
    
    if (substring(spacerRow, rowLength, rowLength) != " ") {
        return (c(0, spaceLocs, rowLength + 1))
    } else {
        return (c(0, spaceLocs))
    }
}


selectCols <- function(colNames, headerRow, searchLocs) {
#' Search for the column names in the header record given the location of all the spaces in the header row and return the locations of columns found. Returns NA is location if colName is not found.
#'
#' \code{selectCols} returns a list containing the header column and the location of the column names mentioned in colNames list.
#'
#' @param colNames - A list of Strings containing all column names to be searched in the header record.
#' @param headerRow - A String in which the presence of each element mentioned in colNames param is to be searched.
#' @param searchLocs - A list containing the location of beginning of every word in the headerrow record.
#' @return a list containing the header row and the starting location of the column names mentioned in colNames list.
#'
#'
    sapply(colNames,
           function(name, headerRow, searchLocs) {
               startPos <- regexpr(name, headerRow)[[1]]
               if (startPos == -1) {
                   return(c(NA, NA))
               }
               
               index <- sum(startPos >= searchLocs)
               c(searchLocs[index] + 1, searchLocs[index + 1])
           },
           headerRow = headerRow, searchLocs = searchLocs)
    
}


extractVariables <- function(file, varNames = c("name", "home", "ag", "gun", "net", "time"),
                             year) {
#' Create a data frame which contains the variables given by the varNames parameter and the data is extracted from data contained in the param file which is a list of string.
#'
#' \code{extractVariables} returns a dataframe.
#'
#' @param file - A list of Strings containing all the data records
#' @param varNames - The name of the columns corresponding to which data is contained in file parameter.
#' @param year - year of the result of the cherry blossom race corresponding to which the data is present in the file parameter.
#' @return a data frame with columns as mentioned in the varNames parameter.
#'
#'

    # Find the index of the row with equal signs
    eqIndex <- grep("^===", file)    

    # Extract the two key rows and the data (fix men 2006 spacer row)
    spacerRow <- file[eqIndex]
    headerRow <- tolower(file[eqIndex - 1])

    if (year == 2006){
        locNetTime <- regexpr("net", headerRow)
        spacerRow <- paste(substr(spacerRow, 1, locNetTime - 2), 
                           substr(spacerRow, locNetTime, nchar(spacerRow)), "")
    }


    body <- file[-c(1:eqIndex)]

    # Obtain the starting and ending positions of variables
    searchLocs <- findColLocs(spacerRow)
    locCols <- selectCols(varNames, headerRow, searchLocs)
    
    Values <- mapply(substr, list(body), start = locCols[1,], stop = locCols[2,])
    colnames(Values) <- varNames
    
    invisible(Values)
}

convertTime <- function(charTime){
#' Convert time from h:mm:ss to minutes.
#'
#' \code{convertTime} return time in minutes.
#'
#' @param charTime - Time in h:mm:ss format
#' @return time in minutes, if the format is other than h:mm:ss, then returns NA.
#'
#'    
    timePieces <- strsplit(charTime, ":")
    timePieces <- sapply(timePieces, as.numeric)
    
    #Fix to account for times that are of incorrect format, e.g. "1:30:" 
    nbrColons <- lapply(charTime, 
                       function(x) {
                         length(gregexpr(":", x)[[1]])
                       })
    
    runTime <- mapply(function(x, y, z){
                  nbrTimePieces <- length(x)
                  if (nbrTimePieces <= y) {
                      return(NA)}
                  else if (nbrTimePieces == 2) {
                      return(x[1] + x[2]/60)}
                  else {
                      return(60*x[1] + x[2] + x[3]/60)}
               }, 
               timePieces, 
               nbrColons,
               charTime)
    
}

createDF <- function(Res, year, sex){
#' Create a dataframe containing the data for the given year for men or women.
#'
#' \code{createDF} return a dataframe .
#'
#' @param Res - Mens/Womens race result data for a year
#' @param year - Year corresponding to which data is supplied
#' @param sex - Specifies if the data is for men or women
#' @return a dataframe which contains data for men/women for the year mentioned.
#'
#'    

    #Determine which time to use
    useTime <- if(!is.na(Res[1, "net"])) {
        Res[, "net"]
    } else if(!is.na(Res[1, "gun"])) {
        Res[, "gun"]
    } else {
        Res[, "time"]}
    
    #Remove # and * and blanks from time
    useTime <- gsub("[#\\*[:blank:]]", "", useTime)
    
    #Drop rows with no time
    Res <- Res[useTime != "", ]
    
    runTime <- convertTime(useTime[useTime != ""])
    
    #convertTime returns NA for invalid run times; drop these records and print
    #message about record(s) dropped
    if(sum(is.na(runTime)) > 0){
      print(paste("Dropping the following records in year", year, "for", 
                  ifelse(sex == "M", "Men", "Women"), 
                  "due to invalid times", sep = " "))
      
      print(Res[is.na(runTime), ])     
    }

    # Create data frame for every year
    Results <- data.frame(year = rep(year, nrow(Res)),
                          sex = rep(sex, nrow(Res)),
                          name = Res[ , "name"],
                          home = Res[ , "home"],
                          age = as.numeric(Res[ , "ag"]), 
                          runTime = runTime,
                          stringsAsFactors = F)
    
    invisible(Results)
                        
}


printRawTableRecords <- function(rawFile, indexToPrint, year, headerRcd=NA) {
#' Print records from the rawFile which includes the header record along with the records at the given indexes
#'
#' \code{printRawTableRecords} returns NULL. This function is just to print some records
#'
#' @param rawFile - A file (list of Strings) that contain the raw cherry blossom results data
#' @param indexToPrint - Data Indexes to print 
#' @param year - Year corresponding to which data is in the rawFile 
#' @param headerRcd - String containing the header information, NULL if header is to be found in the rawFile, should be specified only when there is not header in the raw file.
#' @return NULL. This function is to be used for print purpose only 
#'
#'
    if(is.na(headerRcd)){
        # Find the index of the row with equal signs
        eqIndex <- grep("^===", rawFile)    

        # Extract the two key rows and the data (fix men 2006 spacer row)
        spacerRow <- rawFile[eqIndex]
        headerRow <- tolower(rawFile[eqIndex - 1])
        if (year == 2006){
            locNetTime <- regexpr("net", headerRow)
            spacerRow <- paste(substr(spacerRow, 1, locNetTime - 2), substr(spacerRow, locNetTime, nchar(spacerRow)), "")
        }

        body <- rawFile[-c(1:eqIndex)]

    } else {
        spacerRow <- strrep('=', nchar(headerRcd))
        headerRow <- tolower(headerRcd)
        body <- rawFile
        eqIndex <- 0
    }
    
    cat(headerRow, sep='\n')
    cat(spacerRow, sep='\n')
    for(index in indexToPrint){
        cat(body[index], sep='\n')
    }
}