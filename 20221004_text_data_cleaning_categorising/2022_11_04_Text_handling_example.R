#2022 11 04
#Ed Lefley
#An algorithm enable the Use of R to handle Text data - cleaning, categorising and reporting on findings

#This approach has been used in a scientometric study of over 230,000 journal articles
#To undertake the it was necessary to categorise the author type, and to geolocate their organisation

#Leydesdorff who is one of the pioneers of this apporach states:
#"..studies of international collaboration and triple-helix collaborations, was hitherto not possible because this requires a huge amount of data processing and cleaning of the institutional address information."
#(Leydesdorff, L., & Sun, Y. (2009). National and international dimensions of the Triple Helix in Japan: University-industry-government versus international coauthorship relations. Journal of the American Society for Information Science and Technology, 60(4), 778-788. https://doi.org/10.1002/asi.20997)

#So what does this mean? To undertake this required an algorithmic process to be developed... using a hightech approach.

#From this, the code was written and this session will provide a snapshot into the process and the decision making associated with this.

#Packages to be used:
install.packages('stringi')
install.packages('stringr')

library(readr) #base package, does not need installing
library(stringi)
library(stringr)

#With these two packages, there are very similar functions, but subtle differences in run times - this is important, as one of my final scripts took 2 days (yes, days!) to run - in total to re-run my main model it took 4 days. 
#My preference is to use base functions as much as possible - stringi uses a derivative of C, and therefore is fractionally faster. We will do time comparisons every so often on the functions

#Example Data:
#I have supplied a subset of addresses, that I know contain problems, and will perform some of the processes I have undertaken to clean the address information that Leydesdorff and Sun mention.

#Set Working directory and load csv file
setwd(~)

#Subset out Australia from the addresses - because it's the last field and the addresses are uneven in terms of format, I reverse the addresses so the country is first.

Address <- read_csv("Addresses.csv")
View(Address) #optional

Australia <- str_subset(Address$V1, ", Australia") #Comma is important as it tells the function to look for (what is hopefully) the last term
Australia <- as.data.frame(Australia)

#Now to find some errors
#See how many times UNE has the wrong postcode:
Australia$UNE_Postcode_50 <- stri_detect_regex(Australia$Australia, "UNE.*2350|Univ New England.*2350")
Australia$UNE_Postcode_51 <- stri_detect_regex(Australia$Australia, "UNE.*2351|Univ New England.*2351")
#Now convert from TRUE/FALSE to binary
Australia$UNE_Postcode_50 <- 1*Australia$UNE_Postcode_50
Australia$UNE_Postcode_51 <- 1*Australia$UNE_Postcode_51
Correct_Postcode <- sum(Australia$UNE_Postcode_51)
Incorrect_Postcode <- sum(Australia$UNE_Postcode_50)

#But how bad is that?
str_d_start <-Sys.time()
Australia$UNE <- str_detect(Australia$Australia, "UNE|Univ New England")
str_d_end <-Sys.time()
str_d_time <- str_d_end-str_d_start

stri_d_start <-Sys.time()
Australia$UNE2 <- stri_detect_regex(Australia$Australia, "UNE|Univ New England")
stri_d_end <-Sys.time()
stri_d_time <- stri_d_end-stri_d_start
View(stri_d_time)
View(str_d_time)

#Correcting errors
#Find all examples of Armidale (and spelling mistakes - there's some great ones!)

#We'll use some of the same functions as before:
Armidale <- str_subset(Australia$Australia, "A.*ale, NSW|A.*ale, 235")
Armidale <- as.data.frame(Armidale)

#This time I want to find what isn't Armidale:
Armidale$Wrong <- stri_detect_regex(Armidale$Armidale, "Armidale", negate = TRUE)

#How to correct? Now you know what's wrong, it's a lot easier...
Armidale$Armidale <- gsub("Armadale|Artnidale|Annidale", "Armidale", Armidale$Armidale)
#Quick check:
Armidale$Wrong <- stri_detect_regex(Armidale$Armidale, "Armidale", negate = TRUE)

#Now you can use this same TRUE/FALSE to count how many times a term appears - like Paul has done with his wordcloud:
Armidale$Beef <- stri_detect_regex(Armidale$Armidale, "Beef", negate = FALSE)
Armidale$Law <- stri_detect_regex(Armidale$Armidale, "Law|law", negate = FALSE)
#But R doesn't like counting T/F:
Beef <- sum(Armidale$Beef)
#so binary convert
Armidale$Beef <- 1*Armidale$Beef
Armidale$Law <- 1*Armidale$Law
Beef_B <- sum(Armidale$Beef)
Law_B <- sum(Armidale$Law)