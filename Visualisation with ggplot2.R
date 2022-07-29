
#Syntax created and shared by Brenda Vo, 29/07/2022

rm(list=ls()) # remove all of the existing variables
options(digits=3, show.signif.stars=F) # reduce the digits to 3

# Load libraries
library(MASS)
library(ggplot2)
library(lattice)
library(GGally)

# View dataset cats (accessible within the MASS package)
data(cats)
View(cats)
head(cats)

cats %>%                               # Summary by group using dplyr
  group_by(Sex) %>% 
  summarize(mean = mean(Hwt),
            sd = sd(Hwt),
            median = median(Hwt),
            min = min(Hwt),
            max = max(Hwt)
  )


# plot with ggplot2
ggplot(cats,aes(x=Sex,y=Hwt,color=Sex))+
  geom_boxplot()

# or filled 
ggplot(cats,aes(x=Sex,y=Hwt,fill=Sex))+
  geom_boxplot()

## pairs plot
ggpairs(cats, aes(color = Sex), lower = list(continuous = "smooth"))

## plot of heart weight vs body weight for each gender 

ggplot(cats, aes(Bwt, Hwt, colour= Sex)) + 
  geom_smooth(formula = y ~ x,method = "lm", se = TRUE) + 
  # se = FALSE would delete CIs
  geom_jitter(width = 0.1, height = 0.1) +
  labs(x="Body weight (kg)", y = "Heart weight (g)") +
  facet_wrap(~Sex)

## smooth with polynomial 

ggplot(cats, aes(Bwt, Hwt, colour= Sex)) + 
  geom_smooth(formula = y ~ splines::bs(x, 3),method="lm", se = TRUE) + 
  geom_jitter(width = 0.1, height = 0.1) +
  labs(x="Body weight (kg)", y = "Heart weight (g)") +
  facet_wrap(~Sex)

#Arrange plots using ggpubr() or patchwork()

###############################

library(ISLR2)   # to obtain Hitter dataset
library(ggcorrplot)
library(corrplot)


# baseball player's Salary
View(Hitters)
names(Hitters)
dim(Hitters)

# checking for missing values in Salary
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
summary(Hitters)

# pick numerical variables
HittersN <- subset(Hitters, select=-c(League,Division,NewLeague))


# pairs plot
pairs(HittersN)

## pairs plot using ggplot2
ggpairs(HittersN) ## too busy, can't see anything

# correlation matrix
round(cor(HittersN),digits=2)

######### # heatmap 

#visualize correlation matrix
ggcorrplot(cor(HittersN))

## another way to generate heatmap
corrplot(cor(HittersN), type = "upper", order = "hclust", 
         tl.col = "black")

## heatmap with correlation coefficients together
ggcorr(HittersN,
       method=c("pairwise", "pearson"),
       label = TRUE,
       label_round = 1,
       low = "red", mid="white", high="green",
       hjust = 0.8, size = 4, legend.size = 12) +
  theme(panel.border = element_blank())

## plot with ggplot2
ggplot(Hitters,aes(x=Division,y=Salary,color=NewLeague))+
  geom_boxplot()


