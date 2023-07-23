# Exploring hierarchy in the data

# visualization of order of data

# x and y , both are numeric
#1. barplot
#2. lollipop plot
#3. circular barplot

###############################################################################
#                               1. barplot                                  #
###############################################################################
setwd("C:/SSR-RMIT_laptop/Monash/Misc/INTERSECT/DRA-UNE/R_user_group_presentation")

getwd()  # to see which is our working directory

# reading data from CSV or excel file
# we use iris dataset

#read.csv() function
df= read.csv("iris.csv")
df <- iris
head(df)
summary(df)
str(df)


#read.xlsx() function
df1= readxl::read_xlsx("iris.xlsx")
head(df1)
summary(df1)
str(df1)

# if your data is in specific sheet of excel workbook
df2= readxl::read_xlsx("iris.xlsx",sheet="iris")
head(df2)
summary(df2)
str(df2)

# for the function below we need tidyverse package
library(tidyverse)


#### not needed ######################################
# Selecting column
# In our data, we have 4 columns of numerical values. Therefore, we select these first 4 columns using select function
df_f4= select(df,1:4)
head(df_f4) # we removed the species variable / categorical variable

# filtering rows -  we filter the rows based on species type
df_setosa= filter(df,Species=="setosa")
head(df_setosa)
dim(df_setosa)

df_virginica= filter(df,Species=="virginica")
head(df_virginica)
dim(df_virginica)

#######################################################


# gathering columns to make long table -  we want to gather the sepal length, sepal width, petal length and petal width into a single column
df_long= gather(df,Mes_type,length,1:4) # Mes_type represents another column which determines the measurement type
                                        # length is the column name having the numerical values
head(df_long) # we require this long table to plotting bar plots

# we now need to calculate the mean and sd of petal length, petal width, sepal length and sepal width for each species for our bar plots. We use group_by and summarise fucntions for these



#group by and Summarize
df_sumzd=group_by(df_long,Species,Mes_type) %>% 
  summarise(mean=mean(length),sd=sd(length))
head(df_sumzd)

# Now we have tranformed our data into required format to plot our data

# Plot basic barplot
p=ggplot(df_sumzd,aes(x=Species,y=mean))+
   geom_bar()
p

# This will give us error
# By default stat=count which uses only x- values and create a barplot counting the number of rows of x
# because we have x and y values, we  need to include stat='identity', which is basically telling ggplot2 you will provide the y-values for the barplot along with x values


# Plot basic barplot- effect of stat= "identity"
p=ggplot(df_sumzd,aes(x=Species,y=mean))+
   geom_bar(stat="identity")
p



# fill color

p=ggplot(df_sumzd,
         aes(x=Species,y=mean))+
   geom_bar(stat="identity",fill="red")
p

# fill color - mapping to a variable

p=ggplot(df_sumzd,
         aes(x=Species,y=mean, fill=Species))+
   geom_bar(stat="identity")
p

#### Type A #####
# But I am not happy with this plot as from the plots I cannot differentiate the mean of each measurement type
# There I will use a stacked bar plot here, where I will stack each Mes_type one on top of each for each species
# fill colors- stacked bar plot 
p=ggplot(df_sumzd,
         aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity")
p


#### Type B #####
# If you don't want to put the bars on top of each other, I can graoup them based on the species type as well
# need to add position="dodge"
# fill colors- grouped bar plot 
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge")
p

# After I got my bar plots, I want to plot the error bars as well. Error bars can be calculated in many ways, I have calculated the error bars based on standard deviation (formula is:  mean-sd, mean+sd)
#Error bars - we use geom_bar function
p=ggplot(df_sumzd,
         aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))


p
# These bars are not satisfactory. We will try and customize these

#Error bars width
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25)
p
#Still we can see that the error bars are not in correct position. These are stacked error bars and we don't want like these
# we want to place the error bars on top of each correctly using position_dodge with width

#Error bars width, size,position
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9))
p

# for controlling the transparency, we use alpha
#Error bars width,size, position, alpha
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=.5,position= position_dodge(.9),alpha=0.3)

p


# NOw we want to add labels above each bar, use geom_text and control position of text using vjust and hjust
#Error bars width,size, position, alpha and labels, label position corrected
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   geom_text(position=position_dodge(0.9),vjust=-1)


p



#theme classic with more corrected label position using vjust and hjust
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   geom_text(position=position_dodge(0.9),vjust=-0.5, hjust=1)+
   theme_classic()


p


#theme bw
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   geom_text(position=position_dodge(0.9),vjust=-0.5, hjust=1.1)+
   theme_bw()


p

# I don't like this plot. So I remove the labels

p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
      theme_bw()
p


# if you want to flip the coordinates, like make x axis as y axis and vise versa, use coord_flip()
# coord flip, this to when x axis labels are very long and overlap each other
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   theme_bw()+
   coord_flip()


p

#### Type C #####
### creating percent stack bar chart - using scales library and use scale_y_continuous(labels = percent)
install.packages("scales")
library(scales)
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
  geom_bar(stat="identity",position="fill") +
  scale_y_continuous(labels = percent)
  theme_bw()
p


# for customization with different themes
install.packages("viridis")
library(viridis)
install.packages("hbrthemes")
library(hrbrthemes)

ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
  geom_bar(stat="identity",position="stack") +
  scale_fill_viridis(discrete = T)+
  ggtitle("Studying 3 flower species...")+
  theme_bw()

#### Type D #####
## Facet bar plots - When you want to plot each species separately (use position = "dodge" and facet_wrap fucntion)
ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_viridis(discrete = T,
                     option = "A")+ # option = different colouring options a,b,c,etc. 
  ggtitle("Studying 3 flower species...")+
  facet_wrap(~Species) +
  theme_bw()


##### Type E ####
## Lolipop chart
# it consists of a marker and a stem
# Recreating our data - taking a subset of our iris data
library(tidyverse)
df= read.csv("iris.csv")
df_new <- iris
df_new = select(df,c(1,5))
df_new_long= gather(df,Mes_type,length,1) # extracting only values from column 1
df_new_sumz= group_by(df_new_long,Species,Mes_type) %>% 
  summarise(mean=mean(length))

# this is only for the sepal length of each species
ggplot(df_new_sumz,aes(x=Species,y=mean))+
  geom_segment(aes(x=Species, xend=Species, y=0, yend=mean)) +
  geom_point(
    size = 5, 
    color = "red",
    alpha = 0.6,
    shape = 21, # changes the shape of the marker
    fill = "orange",
    stroke = 2)  # CONTROLS WIDTH OF OUTLINE
  

##### Type F ####
# Circular bar plots
# Such plots are made when we have large number of bars, like more than 12 
# Examples -  plots of number of insect images collected for each species in a family
# I have created a hypothetical data for demonstration purpose

# data creation
letters = letters[1:26]
values = sample(10:100, 26)

df_circ = data.frame(letters, values)

ggplot(df_circ,aes(x=letters,y=values,fill=letters))+
  geom_bar(stat="identity") +
  theme_minimal()+
  theme(legend.position = "none")

# this gives large number of bars
# create circular bar plot
ggplot(df_circ,aes(x=letters,y=values,fill=letters))+
  geom_bar(stat="identity") +
  theme_minimal()+
  theme(legend.position = "none") + 
  coord_polar(start = 0) + # polar coordinates like for pie charts
  ylim(-50, 100)  #we are trying to add ylim at centre - so starting from -50

# ordering the bars in an increasing order, use command reorder
ggplot(df_circ,aes(x=reorder(letters,values),y=values,fill=letters))+
  geom_bar(stat="identity") +
  theme_minimal()+
  theme(legend.position = "none") + 
  coord_polar(start = 0) + # polar coordinates like for pie charts
  ylim(-50, 100)


#That's it for bar plot.