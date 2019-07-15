install.packages("rjson","sqldf","data.table")

library(rjson)
library(sqldf)
library(data.table)

# specify the base directory for input and output
# change this to wherever on your computer you've saved your "splitaa.Rdata" file to
baseDirectory <- "/home/mwatts/Documents/crossref/"

######## Extract affiliation name from JSON data
load(file=paste0(baseDirectory,"splitaa.Rdata"))

# create output affiliation CSV table
options(useFancyQuotes = FALSE)
outFile <- paste0(baseDirectory,"affiliation.csv")
outConnection <- file(outFile,open="wb")
theLine <- "split_file,record,author,affiliation,affiliation_name"
writeLines(theLine,outConnection)

# parse the lines in the sample input data: "splitaa.Rdata"
for (j in 1:length(inData))
{
  # display a progress message every 10000 records
  if ((j %% 10000) == 0)
  {
    cat(" ",j)
  }
  
  # read this line and convert it's JSON content to R
  inLine <- inData[j]
  crossrefData <- fromJSON(inLine)
  
  # process the line if it has author information
  if (length(crossrefData$author) > 0)
  {
    # traverse each author in turn
    for (k in 1:length(crossrefData$author))
    {
      # process this author if they have affiliation information
      if (length(crossrefData$author[[k]]$affiliation) > 0)
      {
        # traverse each affiliation in turn
        for (l in 1:length(crossrefData$author[[k]]$affiliation))
        {
          # get this author affiliation
          theAffiliationName <- dQuote(gsub('"',"'",crossrefData$author[[k]]$affiliation[[l]]$name))
          
          # write this author affiliation to the output CSV table
          theLine <- paste0("splitaa,",         # split_file
                            j,",",              # record
                            k,",",              # author
                            l,",",              # affiliation
                            theAffiliationName) # affiliation_name
          writeLines(theLine,outConnection)
          flush(outConnection)
        }
      }
    }
  }
  cat("\n")
}
close(outConnection)

######## Query affiliation name and extract UNE affiliated JSON records
# load the affiliation CSV file we've just created
crossrefAffiliation <- read.csv(paste0(baseDirectory,"affiliation.csv"),stringsAsFactors=F)

# convert the affiliation names to lower case for easier string matching
lowerAffiliation <- tolower(crossrefAffiliation$affiliation_name)

# grep for UNE affiliations
uneGrep <- sort(unique(c(grep("university of new england",lowerAffiliation),
                         grep("une",lowerAffiliation))))
uneCrossrefAffiliation <- crossrefAffiliation[uneGrep,]

# grep for UNE Armidale affiliations (to filter out the other UNE)
armidaleGrep <- sort(unique(c(grep("armidale",tolower(uneCrossrefAffiliation$affiliation_name)),
                              grep("australia",tolower(uneCrossrefAffiliation$affiliation_name)),
                              grep("new south wales",tolower(uneCrossrefAffiliation$affiliation_name)),
                              grep("nsw",tolower(uneCrossrefAffiliation$affiliation_name)),
                              grep("2351",tolower(uneCrossrefAffiliation$affiliation_name)) )))
uneArmidaleCrossrefAffiliation <- uneCrossrefAffiliation[armidaleGrep,]

uneArmidaleIndices <- sqldf("select distinct record from uneArmidaleCrossrefAffiliation")$record

# select rows that have UNE affiliation from the JSON data
uneArmidaleCrossrefRecords <- inData[uneArmidaleIndices]

length(uneArmidaleCrossrefRecords) # 17

# save UNE Armidale Crossref records
save(uneArmidaleCrossrefRecords,file=paste0(baseDirectory,"uneArmidaleCrossrefRecords.Rdata"))

######## Extract “title” for UNE affiliated JSON records
load(file=paste0(baseDirectory,"uneArmidaleCrossrefRecords.Rdata"))

# parse each UNE Armidale record, extracting some of it's metadata
theTable <- c()
for (i in 1:length(uneArmidaleCrossrefRecords))
{
  cat(i,"\n")
  
  # read this line and convert it's JSON content to R
  inLine <- uneArmidaleCrossrefRecords[i]
  crossrefData <- fromJSON(inLine)
  
  # extract some metadata for this line
  theReferenceCount <- paste0(crossrefData$`reference-count`,collapse="||")
  thePublisher <- paste0(crossrefData$publisher,collapse="||")
  if (crossrefData$type=="journal-article")
  {
    theIssue <- paste0(crossrefData$issue,collapse="||")
  } else {
    theIssue <- ""
  }
  theShortContainerTitle <- paste0(crossrefData$`short-container-title`,collapse="||")
  theDOI <- paste0(crossrefData$DOI,collapse="||")
  theType <- paste0(crossrefData$type,collapse="||")
  thePage <- paste0(crossrefData$page,collapse="||")
  theSource <- paste0(crossrefData$source,collapse="||")
  theIsReferencedByCount <- paste0(crossrefData$`is-referenced-by-count`,collapse="||")
  theTitle <- paste0(crossrefData$title,collapse="||")
  thePrefix <- paste0(crossrefData$prefix,collapse="||")
  theVolume <- paste0(crossrefData$volume,collapse="||")
  
  # create an author string by concatenating together all the author information
  theAuthor <- ""
  if (length(crossrefData$author) >  0)
  {
    for (j in 1:length(crossrefData$author))
    {
      crossrefData$author[[j]]
      if (j > 1)
      {
        theAuthor <- paste0(theAuthor,"||")
      }
      theAuthor <- paste0(theAuthor,crossrefData$author[[j]]$sequence,": ",crossrefData$author[[j]]$family,", ",crossrefData$author[[j]]$given)
      if (length(crossrefData$author[[j]]$affiliation) > 0)
      {
        theAuthor <- paste0(theAuthor,", affiliation: ")
        for (k in 1:length(crossrefData$author[[j]]$affiliation))
        {
          if (k > 1)
          {
            theAuthor <- paste0(theAuthor,", ")
          }
          theAuthor <- paste0(theAuthor,crossrefData$author[[j]]$affiliation[[k]]$name)
        }
      }
    }
  }
  
  # extract some more metadata for this line
  theMember <- paste0(crossrefData$member,collapse="||")
  theContainerTitle <- paste0(crossrefData$`container-title`,collapse="||")
  theScore <- paste0(crossrefData$score,collapse="||")
  
  theIssued <- paste0(unlist(crossrefData$issued$`date-parts`),collapse=" ")
  theDeposited <- crossrefData$deposited$`date-time`
  
  theReferencesCount <- paste0(crossrefData$`references-count`,collapse="||")
  theURL <- paste0(crossrefData$URL,collapse="||")
  theISSN <- paste0(crossrefData$ISSN,collapse="||")
  theSubject <- paste0(crossrefData$subject,collapse="||")
  
  # create a row for the table with this publications metadata
  theRow <- cbind(i,
                  theReferenceCount,thePublisher,theIssue,theShortContainerTitle,
                  theDOI,theType,thePage,theSource,
                  theIsReferencedByCount,theTitle,thePrefix,theVolume,
                  theAuthor,theMember,theContainerTitle,theScore,
                  theIssued,theDeposited,
                  theReferencesCount,theURL,theISSN,theSubject)
  
  # add this row to the table
  theTable <- rbind(theTable,theRow)
}

# give friendly column names to the table
colnames(theTable) <- c("i",
                        "reference_count","publisher","issue","short_container_title",
                        "DOI","type","page","source",
                        "is_referenced_by_count","title","prefix","volume",
                        "author","member","container_title","score",
                        "issued","deposited",
                        "references_count","URL","ISSN","subject")
# cast the table as a data table for convenience later on
uneArmidaleCrossrefPublications <- data.table(theTable)

# save extracted crossref publications
save(uneArmidaleCrossrefPublications,file=paste0(baseDirectory,"uneArmidaleCrossrefPublications.Rdata"))
write.csv(uneArmidaleCrossrefPublications,file=paste0(baseDirectory,"uneArmidaleCrossrefPublications.csv"),row.names=F,na="")
