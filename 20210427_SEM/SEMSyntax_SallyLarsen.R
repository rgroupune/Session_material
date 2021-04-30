
library(lavaan)

setwd ("C:/Users/slarsen3/ownCloud/Documents/Workshops/Lavaan 2021")

#next read data - here if it is stored as a .csv file

Data1 = read.csv("DataName.csv", header=TRUE)

#EXAMPLE 1 #########################################################
#Data from Schreiber et al. (2006) 
 #James B. Schreiber , Amaury Nora , Frances K. Stage , Elizabeth A.
#Barlow & Jamie King (2006) Reporting Structural Equation Modeling and Confirmatory Factor
#Analysis Results: A Review, The Journal of Educational Research, 99:6, 323-338, DOI: 10.3200/
#JOER.99.6.323-338
#To link to this article: https://doi.org/10.3200/JOER.99.6.323-338

#another good article
#Albert WD, Hanson JL, Skinner AT, et
#al. Individual differences in executive function partially explain
#the socioeconomic gradient in middle-school academic
#achievement. Dev Sci. 2020;23:e12937. https ://doi.
#org/10.1111/desc.12937

#You don't need to run this first section if you have a full data set
#Because we are using a correlation matrix as input for this example
#We need to define variable names and means and sds
names <- c("ComCon", "TermP", "Read", "SpecF", "OneA", "OneM", "Ach")

#The variables in this example were standardized hence M=0 and SD=1 for all
EBLP.mns <- c(0, 0, 0, 0, 0, 0, 0)

EBLP.sds <- c(1, 1, 1, 1, 1, 1, 1)

EBLP.cor <- read.csv("EBLP.csv", header=FALSE, row.names=names, col.names=names)
EBLP.cor <-data.matrix(EBLP.cor)
EBLP.cov <- cor2cov(EBLP.cor, sds=EBLP.sds)

###########################################################################
#Model in Figure 5 - CFA model with correlated factors

mod.1 <- 'Deep =~ ComCon + TermP + Read
          Iso =~ SpecF + OneA + OneM'

fit.1 <- sem(mod.1, sample.cov=EBLP.cov, sample.mean=EBLP.mns, sample.nobs = 203,
             meanstructure=TRUE, std.lv=TRUE)
summary(fit.1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#If you have a data frame (rather than a covariance matrix as we do here)
#The syntax for the fit stats is this:
# fit.1 <- sem(mod.1, data=EBLPData, meanstructure=TRUE, std.lv=TRUE)
 

#Model in Figure 6 - Mediation model with two latent factors predicting Achievement

mod.2 <- 'Deep =~ ComCon + TermP + Read
          Iso =~ SpecF + OneA + OneM

          Deep ~ Iso
          Ach ~ Deep + Iso'

fit.2 <- sem(mod.2, sample.cov=EBLP.cov, sample.mean=EBLP.mns, sample.nobs = 194,
             meanstructure=TRUE, std.lv=TRUE)
summary(fit.2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#EXAMPLE 2#######################################################.
#Data from Albert et al. (2020)

#Again using a correlation matrix plus means and sds so define these objects
names2 <- c("PEdu", "Read", "Math", "VbF", "VbWM", "SpWM", "Plan", "Inhb")

EF.mns <- c(13.33, 50.10, 48.86, 9.78, 6.05, 4.51, 0.34, -0.09)

EF.sds <- c(3.85, 32.45, 31.92, 2.59, 1.28, 1.40, 0.11, 0.13)

EF.cor <- read.csv("EFData.csv", header=FALSE, row.names=names2, col.names=names2)
EF.cor <-data.matrix(EF.cor)
EF.cov <- cor2cov(EF.cor, sds=EF.sds)

################################################################################

mod.3 <- 'EF =~ VbWM + SpWM + Plan + Inhb'

fit.3 <- sem(mod.3, sample.cov=EF.cov, sample.mean=EF.mns, sample.nobs = 203,
    meanstructure=TRUE)
summary(fit.3, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#If we had a dataset, rather than a covariance matrix the fit.3 object
#would be coded thus:
# fit.3 <- sem(mod.3,  data=EFData, meanstructure=TRUE)
# std.lv=TRUE - leave this out because we want to see what happens when we set 
#the first factor loading = 1

mod.4 <-'EF =~ VbWM + SpWM + Plan + Inhb

        EF ~ PEdu
        VbF ~ PEdu
        Read ~ PEdu + EF + VbF
        Math ~ PEdu + EF + VbF

        EF ~~ VbF
        Read ~~ Math'

fit.4 <- sem(mod.4, sample.cov=EF.cov, sample.mean=EF.mns, sample.nobs = 203,
             meanstructure=TRUE)
summary(fit.4, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#How do I get the indirect effects?
#Here is an example of a mediation model using only a section of the
#larger model reported by Albert et al.

mod.4a <- ' EF =~ VbWM + SpWM + Plan + Inhb
# direct effect
            Read ~ c*PEdu
# mediator
             EF ~ a*PEdu
             Read ~ b*EF
# indirect effect (a*b)
             ab := a*b
# total effect
             total := c + (a*b)'

fit.4a <- sem(mod.4a, sample.cov=EF.cov, sample.mean=EF.mns, sample.nobs = 203,
             meanstructure=TRUE)
summary(fit.4a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#You can get bootstrapped standard errors but it doesn't work with
#a covariance matrix as data 
#in the fit section include se="bootstrap


###############################################################################
#Here's another example using the same data to demonstrate two things:
#1 how to get modification indices
#2 how to test the improvement in fit of nested models
mod.5 <-'EF =~ VbWM + SpWM + Plan + Inhb

        EF ~ PEdu
        VbF ~ PEdu
        Read ~ EF + VbF
        Math ~ EF + VbF'

       # EF ~~ VbF
       # Read ~~ Math'

fit.5 <- sem(mod.5, sample.cov=EF.cov, sample.mean=EF.mns, sample.nobs = 203,
             meanstructure=TRUE)
summary(fit.5, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#First modification indices
modindices(fit.5, sort.=TRUE, minimum.value=10)

#Next compare nested models
lavTestLRT(fit.4, fit.5)


#So imagine I use the modindices from mod.5
#to add some paths in my model: what happens?
#I've added PEdu as a predictor of Reading
#And a covariance between EF and VbF

mod.6 <-'EF =~ VbWM + SpWM + Plan + Inhb

      EF ~ PEdu
      VbF ~ PEdu
      Read ~ PEdu + EF + VbF
      Math ~ EF + VbF

      EF ~~ VbF'


fit.6 <- sem(mod.6, sample.cov=EF.cov, sample.mean=EF.mns, 
             sample.nobs = 203, meanstructure=TRUE)
summary(fit.6, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Next compare nested models
lavTestLRT(fit.5, fit.6)

#This command is useful if you have problems with differently sized variances
#But again, it doesn't work with covariance matrices as data
varTable(fit.5) 



