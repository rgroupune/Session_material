## R script for ggpubr examples

##script from: https://rpkgs.datanovia.com/ggpubr/index.html

# also look at: http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/

# and https://github.com/kassambara/ggpubr

#Installation and loading
#Install from CRAN as follows:

#install.packages("ggpubr")

#Or, install the latest version from GitHub as follows:
# Install
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

library(ggpubr)

##Distribution (density plot)

# Create some data

set.seed(1234)
wdata = data.frame(sex = factor(rep(c("F", "M"), each=200)),weight = c(rnorm(200, 55), rnorm(200, 58)))
head(wdata, 4)  ## inspect first 4 rows of data frame

#>   sex   weight
#> 1   F 53.79293
#> 2   F 55.27743
#> 3   F 56.08444
#> 4   F 52.65430

# Basic density plot

ggdensity(wdata, x = "weight",
          color = "sex", fill = "sex")

# Density plot with mean lines and marginal rug
# Change outline and fill colors by groups ("sex")
# Use custom colour palette

ggdensity(wdata, x = "weight",
          add = "mean", rug = TRUE,
          color = "sex", fill = "sex",
          palette = c("#00AFBB", "#E7B800"))

##################################################################

## Basic histogram plot

gghistogram(wdata, x = "weight",
            color = "sex", fill = "sex")

## Histogram plot with mean lines and marginal rug            
# Change outline and fill colors by groups ("sex")
# Use custom colour palette

gghistogram(wdata, x = "weight",
            add = "mean", rug = TRUE,
            color = "sex", fill = "sex",
            palette = c("#00AFBB", "#E7B800"))

##################################################################

#Box plots and violin plots
# Load data
data("ToothGrowth")
df <- ToothGrowth
head(df, 4)

#>    len supp dose
#> 1  4.2   VC  0.5
#> 2 11.5   VC  0.5
#> 3  7.3   VC  0.5
#> 4  5.8   VC  0.5

# Basic plot

p <- ggboxplot(df, x = "dose", y = "len",
               color = "dose")
p

# Box plots with jittered points

# Change outline colours by groups: dose
# Use custom colour palette
# Add jitter points and change the shape by groups

p1 <- ggboxplot(df, x = "dose", y = "len",
               color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "jitter", shape = "dose")
p1

## Add p-values comparing groups
# Specify the comparisons you want

my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") ) # sets up location and extent of probabilities
p1 + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)                   # Add global p-value at y=50

## Basic violin plot

ggviolin(df, x = "dose", y = "len", fill = "dose")

## Violin plots with box plots inside
# Change fill colour by groups: dose
# add boxplot with white fill colour

ggviolin(df, x = "dose", y = "len", fill = "dose",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 50)                                      # Add global the p-value 

#####################################################################

#Bar plots
#Demo data set
#Load and prepare data:
  
# Load data
data("mtcars")
dfm <- mtcars
# Convert the cyl variable to a factor
dfm$cyl <- as.factor(dfm$cyl)
# Add the name colums
dfm$name <- rownames(dfm)
# Inspect the data
head(dfm[, c("name", "wt", "mpg", "cyl")])
#>                                name    wt  mpg cyl
#> Mazda RX4                 Mazda RX4 2.620 21.0   6
#> Mazda RX4 Wag         Mazda RX4 Wag 2.875 21.0   6
#> Datsun 710               Datsun 710 2.320 22.8   4
#> Hornet 4 Drive       Hornet 4 Drive 3.215 21.4   6
#> Hornet Sportabout Hornet Sportabout 3.440 18.7   8
#> Valiant                     Valiant 3.460 18.1   6

#Ordered bar plots
#Change the fill colour by the grouping variable "cyl". Sorting will be done globally, but not by groups.

ggbarplot(dfm, x = "name", y = "mpg",
          fill = "cyl",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90           # Rotate vertically x axis texts
)          

## let's look at the graphical parameters
?ggpar

#Sort bars inside each group. Use the argument sort.by.groups = TRUE.

ggbarplot(dfm, x = "name", y = "mpg",
          fill = "cyl",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "asc",           # Sort the value in dscending order
          sort.by.groups = TRUE,      # Sort inside each group
          x.text.angle = 90           # Rotate vertically x axis texts
)

#####################################################################

#Add Regression Line Equation and R-Square to a GGPLOT

#script from https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html

# Simple scatter plot with correlation coefficient and
# regression line

ggscatter(mtcars, x = "wt", y = "mpg", add = "reg.line") +
  stat_cor(label.x = 3, label.y = 34) +
  stat_regline_equation(label.x = 3, label.y = 32)

# Grouped scatter plot

ggscatter(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Species) +
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 4.2)

## r2

ggscatter(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Species) +
  stat_regline_equation(label.y=4.4,
                      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")))

# Polynomial equation

# Demo data
set.seed(4321)
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x, y, group = c("A", "B"),
                      y2 = y * c(0.5,2), block = c("a", "a", "b", "b"))

# Fit polynomial regression line and add labels
formula <- y ~ poly(x, 3, raw = TRUE)
p2 <- ggplot(my.data, aes(x, y2, color = group)) +
  geom_point() +
  stat_smooth(aes(fill = group, color = group), method = "lm", formula = formula) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  theme_bw()
ggpar(p2, palette = "jco")

data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)
head(df[, c("wt", "mpg", "cyl")], 3)

# Basic plot

ggscatter(df, x = "wt", y = "mpg",
          color = "black", shape = 21, size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
)

# loess method: local regression fitting
ggscatter(df, x = "wt", y = "mpg",
          add = "loess", conf.int = TRUE)


# Control point size by continuous variable values ("qsec")
ggscatter(df, x = "wt", y = "mpg",
          color = "#00AFBB", size = "qsec")

# Change colors

# Use custom color palette
# Add marginal rug
ggscatter(df, x = "wt", y = "mpg", color = "cyl",
          palette = c("#00AFBB", "#E7B800", "#FC4E07") )

# Add group ellipses and mean points
# Add stars

ggscatter(df, x = "wt", y = "mpg",
          color = "cyl", shape = "cyl",
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ellipse = TRUE, mean.point = TRUE,
          star.plot = TRUE)

# Textual annotation

df$name <- rownames(df)
ggscatter(df, x = "wt", y = "mpg",
          color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          label = "name", repel = TRUE)

###################################################################################

# Draw a Textual Table

## R script from: https://rpkgs.datanovia.com/ggpubr/reference/ggtexttable.html

# data
df <- head(iris)

# Default table
# Remove row names using rows = NULL
ggtexttable(df, rows = NULL)

# Blank theme
ggtexttable(df, rows = NULL, theme = ttheme("blank"))

# light theme
ggtexttable(df, rows = NULL, theme = ttheme("light"))

# Column names border only
ggtexttable(df, rows = NULL, theme = ttheme("blank")) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)

# Medium blue (mBlue) theme
ggtexttable(df, rows = NULL, theme = ttheme("mBlue"))

# Customize the table as you want
ggtexttable(df, rows = NULL,
            theme = ttheme(
              colnames.style = colnames_style(color = "white", fill = "#8cc257"),
              tbody.style = tbody_style(color = "black", fill = c("#e8f3de", "#d3e8bb"))
            )
)

# Use RColorBrewer palette
# Provide as many fill color as there are rows in the table body, here nrow = 6
ggtexttable(df,
            theme = ttheme(
              colnames.style = colnames_style(fill = "white"),
              tbody.style = tbody_style(fill = get_palette("RdBu", 6))
            )
)

# Combine density plot and summary table

# Density plot of "Sepal.Length"
density.p <- ggdensity(iris, x = "Sepal.Length",
                       fill = "Species", palette = "jco")

# Draw the summary table of Sepal.Length
# Descriptive statistics by groups
stable <- desc_statby(iris, measure.var = "Sepal.Length",
                      grps = "Species")
stable <- stable[, c("Species", "length", "mean", "sd")]
stable.p <- ggtexttable(stable, rows = NULL,
                        theme = ttheme("mOrange"))

# Arrange the plots on the same page
ggarrange(density.p, stable.p,
          ncol = 1, nrow = 2,
          heights = c(1, 0.5))

#############################################################

#HISTOGRAM WITH DENSITY CURVE IN R USING SECONDARY Y-AXIS
# R script from: https://www.datanovia.com/en/blog/ggplot-histogram-with-density-curve-in-r-using-secondary-y-axis/

library(cowplot)  ## needed for part 2.

set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
head(wdata)

# Basic histogram without the density curve
gghistogram(
  wdata, x = "weight", 
  fill = "sex", palette = c("#00AFBB", "#E7B800")
)

# Add the density curve on the same axis
gghistogram(
  wdata, x = "weight", y = "..density..",
  add = "mean", rug = TRUE,
  fill = "sex", palette = c("#00AFBB", "#E7B800"),
  add_density = TRUE
)

# 1. Create the histogram plot
phist <- gghistogram(
  wdata, x = "weight", 
  add = "mean", rug = TRUE,
  fill = "sex", palette = c("#00AFBB", "#E7B800")
)

# 2. Create the density plot with y-axis on the right
# Remove x axis elements
pdensity <- ggdensity(
  wdata, x = "weight", 
  color= "sex", palette = c("#00AFBB", "#E7B800"),
  alpha = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
  theme_half_open(11, rel_small = 1) +  #### theme_half_open from cowplot
  rremove("x.axis")+
  rremove("xlab") +
  rremove("x.text") +
  rremove("x.ticks") +
  rremove("legend")

# 3. Align the two plots and then overlay them.
aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")
ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

### A couple of good web sites to visit:

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#annotate-the-arranged-figure

# https://www.datanovia.com/en/lessons/combine-multiple-ggplots-into-a-figure/