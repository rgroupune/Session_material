#' Author: Mohammad H. Ferdosi
#' Animal Genetics and Breeding Unit
#' University of New England
#'##############################################################################
A <- matrix(1:4, 2)  # Create a 2 by 2 matrix
class(A)  # Return the class of object A 
B <- list(1:4)  # Create a list
class(B)

data()  # List the available data sets
cars    # One of the data-set available in R
head(cars)

class(cars)
x11()
plot(cars)  # Plot cars data based on the data.frame class
x11()
plot(as.ts(cars$dist))  # Plot cars data based on the ts (time series) class
x11()
par(mfrow = c(2, 2))
plot(lm(cars$dist ~ cars$speed))  # Plot cars data based on the lm (linear models) class

methods(plot)  # Listing all available methods

isS3stdGeneric(plot)  # Check if the plot function is a generic S3 class
isS3stdGeneric(mean)
isS3stdGeneric(summary)
isS3stdGeneric(ls)

# Add a new method to a generic class
library(hsphase)
plot.hsphase <- function(bmh)
{
	imageplot(bmh)
}
data(genotypes)
data(map)
data(pedigree)

halfsib <- hss(pedigree, genotypes)
halfsib <- cs(halfsib, map)
halfsib <- halfsib[[1]]
blockMat <- bmh(halfsib)
class(blockMat)
x11()
plot(blockMat)  # For Windows OS you can also use windows()
class(blockMat) <- "hsphase"
x11()
plot(blockMat)

# Simulate genotype and GC - Toy Example
set.seed(1)  # In order to generate the same random numbers
geno <- matrix(sample(c(0, 1, 2), 28, T), nrow = 4)  # Simulate a very simple genotype data
rownames(geno) <- letters[1:4]



set.seed(1)
gc <- round(matrix(runif(28, 0, 1), nrow = 4), 2)  # Simulate a very simple gc matrix
rownames(gc) <- letters[1:4]


# Create your first S3 classes - genome and GC
genotype <- list(genotype = geno)
class(genotype) <- "genome"
class(genotype)

GC <- list(GC = gc)
class(GC) <- "GenCall"
class(GC)

# Method dispatch
rowStat <- function(myObject)
{
	UseMethod("rowStat", myObject)
}
rowStat.default <- function(myObject)
{
	stop("Undefined Object!")
}
rowStat.genome <- function(myObject)
{
	res <- list()
	res$FRQ <- apply(myObject$genotype, 1, function(x)
			{
				x <- factor(x, levels = c(0, 1, 2))
				table(x)
			})
	res$geno <- myObject
	return(res)
}
rowStat.GenCall <- function(myObject)
{
	# You need to check if you can use rowMeans on myObject
	res <- list()
	res$mean <- round(rowMeans(myObject$GC), 2)
	res$GC <- myObject
	return(res)
}

rowStat(geno)  # Show an error
rowStat(genotype)
rowStat(GC)

set.seed(1)
# Simulate a very simple phenotype
Phenotype <- data.frame(ID = letters[1:4], Age = sample((1:10), 4), color = c("brown", "white", "black", "gray"))

source("./JoinUp.R")  # Loading joinUp - an S3 class
MyData <- as.JoinedUp(Phenotype, geno, gc)

class(MyData)

MyData[, ]

MyData[1:2, 1:2]  # '[' has been redefined!
MyData[ ,1:2]  # '[' has been redefined!

dim(MyData)
dimnames(MyData)
MyData <- gc_max(MyData, 0.5)
rbind(MyData, gc_max(MyData, 0.5))
summary(MyData)


# S4 class defining 
setClass("Genotype", slots = representation(name = "character", geno = "matrix", gc = "matrix"))

Sheep_1 <- new("Genotype", name = "mySheep", geno = geno, gc = gc)  # Low-level constructor - it should be wrapped in a function
Genotype <- function(name, geno, gc)
{
	new("Genotype", name = "mySheep", geno = geno, gc = gc)
}

(A_Sheep <- Genotype("mySeSheep", geno + 1, gc = gc/2))  # A better way to make an instance of an object
(B_Sheep <- new("Genotype", name = "mySeSheep", geno = geno + 1, gc = gc/2))  # The marker can not become 3!



Sheep_1@name
Sheep_1@geno
Sheep_1@dob  # The dob (date of birth) has not defined!


Sheep_3 <- new("Genotype", name = "myThSheep")
Sheep_3@geno <- geno
Sheep_3@geno <- 2  # Error: 2 is not a matrix
slot(Sheep_3, "geno")
class(Sheep_3)

# Set default values for the class member
setClass("Genotype", representation(name = "character", geno = "matrix", gc = "matrix"), prototype = list(name = "graySheep", geno = 0, gc = 0))  #  Similar error!

setClass("Genotype", representation(name = "character", geno = "matrix", gc = "matrix"), prototype = list(name = "graySheep", geno = matrix(0), gc = matrix(0)))

Sheep_4 <- new("Genotype")

# Validate the members
setClass("Genotype", representation(name = "character", geno = "matrix", gc = "matrix"), prototype = list(name = "graySheep", geno = matrix(0), gc = matrix(0)), 
		validity = function(object)
		{
			if (any(object@gc < 0 | object@gc > 1))
			{
				print(range(object@gc))
				return("Error: The gc must be between 0 and 1!")
			}
			if (sum(is.na(object@geno)) == 0) # Not a good practice! Why?
				if (any(object@geno != 0 & object@geno != 1 & object@geno != 2))
				{
					print(table(as.vector(object@geno)))
					return("Error: The genotype must 0, 1 or 2!")
				}
			if (nrow(object@geno) != nrow(object@gc))
			{
				return("Error: The number of rows in genotype and gc must be the same!")
			}
		})

Sheep_1 <- new("Genotype", name = "mySheep", geno = gc, gc = gc)  # Validating the genotype!
Sheep_1 <- new("Genotype", name = "mySheep", geno = t(geno), gc = gc)  # Validating the genotype!
Sheep_1@geno <- t(geno)  # Never do this - it is working without validation,
validObject(Sheep_1)  # Although you can check it later! 


# In general you should use accessors (setters and getters/Mutator) to control the changes in members

# Inheritance
setClass("Merino", representation(woolWeight = "numeric"), contains = "Genotype")  # Type double is not working? Why?
dolly <- new("Merino", woolWeight = 3, geno = geno, gc = gc)

setClass("Suffolk", representation(weight = "numeric"), contains = "Genotype")  
dolly_2 <- new("Suffolk", weight = 3, geno = geno, gc = gc)  # Do you know a better name for sheep:)?



# Defining methods and functions - First set the generic function then define the method
setGeneric("qualityControl", function(object) standardGeneric("qualityControl"))

setMethod("qualityControl", "Genotype", function(object)
		{
			object@geno[object@gc < 0.6] <- NA
			object
		})
(dolly <- qualityControl(dolly))
(dolly_2 <- qualityControl(dolly_2))
(Sheep_1 <- qualityControl(Sheep_1))


# Accessors changing and recalling the name

setGeneric("name", function(x) standardGeneric("name"))
setMethod("name", "Genotype", function(x) x@name)

name(dolly)

setGeneric("name<-", function(x, value) standardGeneric("name<-"))
setMethod("name<-", "Genotype", function(x, value)
		{
			x@name <- value
			validObject(x)
			x
		})

name(dolly) <- "A Sheep!"
name(dolly)

# Changing the genotype
setGeneric("genotype", function(object) standardGeneric("genotype"))
setMethod("genotype", "Genotype", function(object) object@geno)


setGeneric("genotype<-", function(object, value) standardGeneric("genotype<-"))
setMethod("genotype<-", "Genotype", function(object, value)
		{
			print(value)
			object@geno <- value
			validObject(object)
			object
		})

genotype(dolly_2)
genotype(dolly_2) <- matrix(sample(c(0, 1, 2), 28, T), nrow = 4)


isS4(dolly_2)
is(dolly_2)
slotNames(dolly_2)
showMethods(class = "Genotype", printTo = TRUE)
