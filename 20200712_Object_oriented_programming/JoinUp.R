#' Author: Mohammad H. Ferdosi
#' Animal Genetics and Breeding Unit
#' University of New England
###############################################################################


#' S3 class for wrapping genotype gc and link file
#' @param link_file a file that may contain pedigree and phenotypes
#' @param genotype genotype file
#' @param gc  gc file
#' @return JoinedUp object
#' 
#' @author Mohammad H. Ferdosi (based on the valuable response of Nick Kennedy in stack overflow)
#' @export
as.JoinedUp <- function(link_file, genotype, gc)
{
	stopifnot(is.data.frame(link_file), is.matrix(genotype), nrow(link_file) == nrow(genotype), nrow(genotype) == nrow(gc))
	stopifnot(rownames(genotype) == rownames(gc))
	stopifnot(colnames(genotype) == colnames(gc))
	stopifnot(max(as.vector(gc)) <= 1)
	alleles <- unique(as.vector(genotype))
	if (length(alleles) > 4)
	{
		print(alleles)
		warning(paste("There are more that 3 types of allels plus NAs"))
	}
	
	x <- list(link_file = link_file, genotype = genotype, gc = gc)
	class(x) <- "JoinedUp"
	return(x)
}

`[.JoinedUp` <- function(x, i = NULL, j = NULL)
{
	if (is.null(i))
	{
		i <- 1:nrow(x$link_file)
	}
	if (is.null(j))
	{
		j <- 1:ncol(x$genotype)
	}
	
	if (is.character(j) && is.character(i))
	{
		x$link_file <- x$link_file[i, intersect(j, colnames(x$link_file)), drop = FALSE]
		x$genotype <- x$genotype[i, intersect(j, colnames(x$genotype)), drop = FALSE]
		x$gc <- x$gc[i, intersect(j, colnames(x$gc)), drop = FALSE]
	} else if ((is.numeric(i) || is.logical(i)) && (is.numeric(j) || is.logical(j)))
	{
		if (any(is.na(i)) || any(is.na(j)))
		{
			i[is.na(i)] <- FALSE
			j[is.na(j)] <- FALSE
			print("----------------  NAs has become FALSE -----------------------")
		}
		if (sum(i) > 0 && sum(j) > 0)
		{
			x$genotype <- x$genotype[i, j, drop = FALSE]
			x$gc <- x$gc[i, j, drop = FALSE]
			x$link_file <- x$link_file[i, , drop = FALSE]
		} else
		{
			stop("----------------------- Index cannot be zero -------------------------")
		}
		
	} else
	{
		stop("----------------------- Function type error -------------------------")
	}
	x
}

`[<-.JoinedUp` <- function(x, i = NULL, j = NULL, value)
{
	if (is.null(j))
	{
		j <- 1:ncol(x$genotype)
	}
	if (is.null(i))
	{
		i <- 1:nrow(x$link_file)
	}
	stopifnot(is.character(j))
	if (!is.matrix(value) & !is.data.frame(value))
	{
		value <- as.data.frame(t(value), stringsAsFactors = FALSE)
	}
	stopifnot(ncol(value) == length(j))
	if (any(j %in% colnames(x$link_file)))
	{
		df_cols <- intersect(j, colnames(x$link_file))
		x$link_file[i, df_cols] <- value[, match(df_cols, j)]
	}
	if (any(j %in% colnames(x$genotype)))
	{
		mat_cols <- intersect(j, colnames(x$genotype))
		x$genotype[i, mat_cols] <- data.matrix(value[, match(mat_cols, j)])
		x$gc[i, mat_cols] <- data.matrix(value[, match(mat_cols, j)])
	}
	x
}


dim.JoinedUp <- function(x)
{
	print(paste("Link file:", paste(dim(x$link_file), collapse = " ")))
	print(paste("Genotype:", paste(dim(x$genotype), collapse = " ")))
	return(paste("GC:", paste(dim(x$gc), collapse = " ")))
}



rbind.JoinedUp <- function(y, z)
{
	x <- NULL
	if (class(y) == "JoinedUp" && class(z) == "JoinedUp")
	{
		x$genotype <- rbind(y$genotype, z$genotype)
		x$gc <- rbind(y$gc, z$gc)
		x$link_file <- rbind(y$link_file, z$link_file)
		class(x) <- "JoinedUp"
		x
	}
}
dimnames.JoinedUp <- function(x)
{
	dimnames(x$genotype)
}
gc_max <- function(x, max)
{
	if (is.function(x) && is.null(attr(x, "class")))
	{
		list(x, m)
	} else UseMethod("gc_max")
}
gc_max.JoinedUp <- function(x, max)
{
	x$genotype[x$gc < max] <- NA
	print(summary(as.vector(x$gc[!is.na(x$genotype)])))
	x
}

summary.JoinedUp <- function(x)
{
	print("GC Summary:")
	print(summary(as.vector(x$gc[!is.na(x$genotype)])))
	print("Allele Summary:")
	print(table(as.vector(x$genotype), useNA = "ifany"))
	print("Link file summary")
	print(str(x$link_file))
	print("Missing summary SNPs")
	print(summary(colMeans(is.na(x$genotype))))
	print("Missing summary individuals")
	print(summary(rowMeans(is.na(x$genotype))))
}


