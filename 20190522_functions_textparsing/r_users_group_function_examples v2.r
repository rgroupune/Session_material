###########################################################################
## R Users Group - Functions (and some text handling)
###########################################################################
library(ggplot2)

## Change the following line or it won't work!
setwd("~/Bioinformatics and statistics/R Users Group UNE/2019-05-22 - Functions and text handling")

###########################################################################
## Introduction
###########################################################################
## R has many built-in functions, as well as those in specialised packages.
## You can also write your own custom functions.
## The most common need for custom functions is with 'apply', 'sapply', etc.

## To define a function, the function 'function' takes a list of arguments, 
## then an expression based on those arguments. To run that function, you need
## to provide the argument/s, and R will evaluate the expression given that information.

## Here's a trivial function. 'x' is the argument, and 'mean(x)' is the expression.
myfunc=function(x)mean(x,na.rm = T)
myfunc(1:5)
myfunc(-3:6)

## This function is trivial because there's already a function that does this...'mean'.

## Here is a less trivial function, using nested functions.
myfunc=function(x)mean(abs(x-mean(x)))
myfunc(1:5) 
myfunc(-3:6) 

## You can use multiple arguments, and they can be of different types.
## If your expression runs over more than one line, use {}
myfunc=function(x,square){
  if(square)
    mean((abs(x-mean(x)))^2)
  else
    mean(abs(x-mean(x)))
}
myfunc(1:5,FALSE)
myfunc(1:5,TRUE)


###########################################################################
## Applications of functions
###########################################################################
## 1. 'apply', 'do.call', 'sapply', 'lapply' etc.
##  These tools will run a function repeatedly on each item of a data structure, 
##  such as a vector, a data frame, or a list. The function does not have to be named.
## E.g.
## Making a dummy data frame- 'sapply' tries to fit the results
##  into a vector
mydata=sapply(1:5,function(x)sample(1:100,50))
str(mydata)
## Extracting information from that data frame using 'apply'
length(which(mydata>=50))
apply(mydata,1,function(x)length(which(x>=50))) # x is one of the vectors comprising mydata

apply(mydata,1,function(x)sum(x>=50)) # x is one of the vectors comprising mydata
apply(mydata,2,function(x)sum(x>=50)) # x is one of the vectors comprising mydata

##or
sapply(1:nrow(mydata),function(x)length(which(mydata[x,]>=50))) # x is a row number

## 2. Automating frequently-used processes - named functions
##  More repeatable than copying code.
##  Tends to be faster than loops 

## Example: output files from population genetics software, STRUCTURE
##  This software produces many output files containing a lot of information. 
##  Commonly, we want to extract some information from each, but the format
##  differs slightly among files.

filedir="snps_10x_10k"
filelist=list.files(filedir)

## OR...
# filedir="."
# filelist=list.files(filedir,"_f$")

## Function to count the number of lines in the file
nlines.in.file=function(a)length(scan(a,what="character",blank.lines.skip = FALSE,sep="\n"))
x=scan(file.path(filedir,filelist[1]),what="character",blank.lines.skip = FALSE,sep="\n")

nlines.in.file(file.path(filedir,filelist[1]))
nlines.in.file(file.path(filedir,filelist[11]))
nlines.in.file(file.path(filedir,filelist[21]))

## Create data frame to summarise file information
filesummary<-data.frame(filename=filelist,
                       nlines=sapply(file.path(filedir,filelist),
                                     nlines.in.file))

## check the distribution of file lengths
table(filesummary$nlines)

## Extract data frame from a file - several steps
n=31
myfile=file.path(filedir,filelist[n])

filecontents=scan(myfile,"character",sep="\n",blank.lines.skip = FALSE)
refline=grep("Inferred ancestry",filecontents)
nind=unlist(read.table(myfile,
                   skip=grep("Run parameters",filecontents),
                   blank.lines.skip = FALSE,
                   header=FALSE,
                   nrows = 1)[1])
mydata=read.table(myfile,
                  skip=refline+1,
                  blank.lines.skip = FALSE,
                  header=FALSE,
                  stringsAsFactors = FALSE,
                  nrows = nind)
head(mydata) ## column 5 is just colons, so we'll delete that
mydata=mydata[,-5]
names(mydata)=c("ID","Ind","%miss","pop","Cluster1", "Cluster2","Cluster3", "Cluster4", "CI1", "CI2","CI3", "CI4")

## One step (after function is defined)

## Define function - not annotated, sorry!
read.struc.out=function(thisfile){
  filecontents=scan(thisfile,"character",sep="\n",blank.lines.skip = FALSE)
  startline=grep("Inferred ancestry",filecontents)+2
  stopline=grep("Estimated Allele Frequencies",filecontents)-2
  qframe=read.table(thisfile,
                    skip=startline-1,
                    blank.lines.skip = FALSE,
                    header=FALSE,
                    stringsAsFactors = FALSE,
                    nrows = stopline-startline)[,-5]
  nclusters=(ncol(qframe)-4)/2
  names(qframe)=c("ID","Ind","%miss","Pop",paste0("Cluster",1:nclusters),
                  paste0("CI",1:nclusters))
  qframe$Pop=factor(qframe$Pop)
  return(qframe)
}

## Run function
mydata=read.struc.out(file.path(filedir,filelist[11]))
mydata=read.struc.out(file.path(filedir,filelist[31]))
tail(mydata)


## 3. Plotting
##  It can be useful to define a function if you need to do the same type of
##  plot for several different variables.

ggplot(mydata,aes(ID,Cluster1,col=factor(Pop)))+geom_point()

## Define a function - note that 'aes_string' is used instead of 'aes". 
## You can also try 'aes_'.
plotq=function(column,dframe)ggplot(dframe,aes_string("ID",column,col="Pop"))+
  geom_point()

plotq("Cluster1",mydata)
plotq("Cluster2",mydata)
plotq("Cluster3",mydata)

## And you can add to them (this example is meaningless, but looks pretty)
plotq("Cluster1",mydata)+geom_smooth()

## 4....
## 5....

###########################################################################
## Best practice for functions
###########################################################################
## 1. Named functions will be more applicable if the arguments include all of the
## required information. 
## Bad:
myfunc=function(x)x/y
myfunc(1:49)
y=101:149
myfunc(1:49)
## Better:
myfunc=function(x,y)x/y

## 2.....
## 3.....
