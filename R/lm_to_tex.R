#' lm model to TeX
#'
#' This function takes a list of lm models and outputs TeX code to create a pretty table.
#'
#' In this version, we start simple. The list can only contain a single model (not
#' much of a list). We output the TeX code to the console.
#' 
#' Plans for later features:
#' 1. Take list of models and put them in the same table.
#' 2. Output to a file
#' 3. Compile and show pdf
#'
#' @param models List of lm models.
#' @param confint Logical if \code{TRUE} adds confidence interval to the table.
#' @param fit Logical if \code{TRUE} adds fit information (R^2) to the table.
#' @param sample.size Logical if \code{TRUE} adds sample size to the table.
#' @return chr TeX code for the table
#' @export
#' @examples
#' lm_to_tex(list(model1, model2))
#' lm_to_tex(list(model1), confint=F, fit=F, sample.size=F)
lm_to_tex <- function(models, confint=T, fit=T, sample.size=T) {
	#Assume 1 model for now
	model <- models[[1]]
	
	print("Test")
}