# Function to extract the overall ANOVA p-value out of a linear model object
# Source: http://gettinggeneticsdone.blogspot.ca/2011/01/rstats-function-for-extracting-f-test-p.html
lmp <- function (modelobject) {
	if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
	f <- summary(modelobject)$fstatistic
	p <- pf(f[1],f[2],f[3],lower.tail=F)
	attributes(p) <- NULL
	return(p)
}

# Round decimals
# Source: http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)