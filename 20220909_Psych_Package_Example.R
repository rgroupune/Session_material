#Example syntax for psych() package Sally Larsen, September 9, 2022
citation(package = "psych")

#Excellent documentation here - plus vignettes https://cran.r-project.org/web/packages/psych/psych.pdf

#I'm using some non-sharable data today, but I hope this syntax is easily transferable

#First set directory and read in data

setwd("C:/Users/slarsen3/ownCloud/Research Assistant 2021/DASS")
DASS2=read.csv("Cleaned_DASS_Data_010422.csv", header=T)

#install.packages("psych")

require(psych)
library(psych)
#--------------------------------------------------------------------------------
#First descriptive statistics 

#Complete dataset
describe(DASS2)

#subset 21 DASS items
DASS21 <- subset(DASS2, select=c(25:45))

describe(DASS21)

#subset 8 items for Q4
Q4 <- subset(DASS2, select=c(46:53))
describe(Q4)

#First make a data frame containing the descriptives
descriptives <- describe(Q4)

#Save out descriptives as a csv
library(utils)
write.csv(descriptives,
          "C:/Users/slarsen3/OneDrive - University of New England/Documents/R//Q4_descriptives.csv", 
          row.names = FALSE)

#-------------------------------------------------------------------------------
#Correlations

#DASS21 items
lowerCor(DASS21, use="complete.obs", method="pearson")

corr.test(DASS21, y = NULL, use = "pairwise",method="pearson",
          alpha=.05, ci=FALSE, minlength=5)

#Question 4 items
lowerCor(Q4, use="complete.obs", method="pearson")

corr.test(Q4, y = NULL, use = "pairwise", method="pearson",
          alpha=.001, minlength=5, ci=TRUE)


#---------------------------------------------------------------------------------
#Factor analysis (ok PCA)

#PCA for the 8 items in Question 4 - a straightforward example

scree1 <- principal(Q4, nfactors=1, scores=TRUE)
plot(scree1$values, ylab = 'Eigenvalue')

#Next generate a dataframe containing the eigenvalues to inspect them

scree1.df = data.frame(values=scree1$values,
                        percent.var = prop.table(scree1$values),
                        cum.percent = cumsum(prop.table(scree1$values)))

print(scree1.df)

#Next a parallel analysis to compare with the information we get out of the 
#eigenvalues and scree plot

scree2 <- fa.parallel(Q4, nfactors = 1, fa = 'pc')

#Great! One factor solution seems pretty defensible

#We'll check now a one-factor EFA using maximum likelihood estimation

ml <- fa(Q4, nfactors = 1, fm='ml')
print(ml)

#---------------------------------------------------------------------------------
#Reliability of the Q4 scale

omega(Q4)


#------------------------------------------------------------------------------
#A second more complicated example
#--------------------------------------------------------------------------------
#PCA of all 21 items in the Depression, Anxiety and Stress Scale (DASS)

#First force a one factor solution and plot eigenvalues in a scree plot

scree1b <- principal(DASS21, nfactors=1, scores=TRUE)
plot(scree1b$values, ylab = 'Eigenvalue')

#Next generate a dataframe containing the eigenvalues to inspect them

scree1b.df = data.frame(values=scree1b$values,
                       percent.var = prop.table(scree1b$values),
                       cum.percent = cumsum(prop.table(scree1b$values)))

print(scree1b.df)

#Next a parallel analysis to compare with the information we get out of the 
#eigenvalues and scree plot

scree2b <- fa.parallel(DASS21, nfactors = 1, fa = 'pc')

#require(GPArotation) - psych will do this for you
#oblimin transformation is the default (oblique) rotation

#If we decided to retain a 3 factor solution

pca1 <- principal(DASS21, nfactors=3, rotate = "oblimin")
print(pca1)

#if you want to do an orthogonal rotation (which you probably don't)

pca2 <- principal(DASS21, nfactors=3, rotate = "varimax")
print(pca2)

#Here is a true factor model (i.e. EFA) using Maximum likelihood estimation

ml1 <- fa(DASS21, nfactors = 3, rotate = "oblimin", SMC=TRUE, fm='ml')
print(ml1)

#What if we force it to be a one factor model?
ml2 <- fa(DASS21, nfactors = 1, SMC=TRUE, fm='ml')
print(ml2)

#-------------------------------------------------------------------------------
#Reliability - Alpha / Omega etc

#First subset the items for each of the three factors

Depression <- subset(DASS21, select=c(3,5,10,13,16,17,21))
Anxiety <- subset(DASS21, select=c(2,4,7,9,15,19,20))
Stress <- subset(DASS21, select=c(1,6,8,11,12,14,18))

omega(Depression)
omega(Anxiety)
omega(Stress)

