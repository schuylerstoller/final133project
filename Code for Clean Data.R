# Link to view published document 
# https://docs.google.com/spreadsheets/d/1otIq7MRAbJ0asEecKjFG2eRbn564iA_dOj0CaBgxlz4/pubhtml

# Reading the Data in From Google Sheets (Published)
install.packages("XML")
library(XML)

Google_Doc<-"https://docs.google.com/spreadsheets/d/1otIq7MRAbJ0asEecKjFG2eRbn564iA_dOj0CaBgxlz4/pubhtml"

CleanGoogleTable <- function(dat, table=1, skip=0, ncols=NA, nrows=-1, header=TRUE, dropFirstCol=NA){
  if(!is.data.frame(dat)){
    dat <- dat[[table]]
  }
  if(is.na(dropFirstCol)) {
    firstCol <- na.omit(dat[[1]])
    if(all(firstCol == ".") || all(firstCol== as.character(seq_along(firstCol)))) {
      dat <- dat[, -1]
    }
  } else if(dropFirstCol) {
    dat <- dat[, -1]
  }
  if(skip > 0){
    dat <- dat[-seq_len(skip), ]
  }
  if(nrow(dat) == 1) return(dat)
  if(nrow(dat) >= 2){
    if(all(is.na(dat[2, ]))) dat <- dat[-2, ]
  }
  if(header && nrow(dat) > 1){
    header <- as.character(dat[1, ])
    names(dat) <- header
    dat <- dat[-1, ]
  }
  # Keep only desired columns
  if(!is.na(ncols)){
    ncols <- min(ncols, ncol(dat))
    dat <- dat[, seq_len(ncols)]
  }
  # Keep only desired rows
  if(nrows > 0){
    nrows <- min(nrows, nrow(dat))
    dat <- dat[seq_len(nrows), ]
  }
  # Rename rows
  rownames(dat) <- seq_len(nrow(dat))
  dat
}

ReadGoogleSheet <- function(url, na.string="", header=TRUE){
  stopifnot(require(XML))
  # Suppress warnings because Google docs seems to have incomplete final line
  suppressWarnings({
    doc <- paste(readLines(url), collapse=" ")
  })
  if(nchar(doc) == 0) stop("No content found")
  htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
  ret <- readHTMLTable(htmlTable, header=header, stringsAsFactors=FALSE, as.data.frame=TRUE)
  lapply(ret, function(x){ x[ x == na.string] <- NA; x})
}


CleanTable1<-ReadGoogleSheet(Google_Doc)
Cleaned_Data_Frame <- CleanGoogleTable(CleanTable1, table=1)

no_repeats <- Cleaned_Data_Frame[!duplicated(Cleaned_Data_Frame), ]
  



