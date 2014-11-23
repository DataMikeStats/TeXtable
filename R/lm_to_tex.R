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

#' lm model to TeX
#'
#' This function takes a list of lm models and outputs TeX code to create a pretty table.
#'
#' In this version, we start simple. The list can only contain a single model (not
#' much of a list). We output the TeX code to the console.
#' 
#' Plans for later features:
#' 1. Take list of models and put them in the same table.
#' 2. Compile and show pdf
#' 3. Truncate decimal places
#'
#' @param file chr file name for output.
#' @param models List of lm models.
#' @param confint Logical if \code{TRUE} adds confidence interval to the table.
#' @param fit Logical if \code{TRUE} adds fit information (R^2) to the table.
#' @param sample.size Logical if \code{TRUE} adds sample size to the table.
#' @param decimals int Number of decimal places.
#' @return NULL. Just outputs the table as a file.
#' @export
#' @examples
#' lm_to_tex("output.tex",list(model1, model2))
#' lm_to_tex("output.tex",list(model1), confint=F, fit=F, sample.size=F)
lm_to_tex <- function(file, models, confint=T, fit=T, sample.size=T, decimals=2) {
	
	k= decimals

	#Create start of file
	lines = c("\\documentclass[]{article}")
	lines = c(lines,"\t\\usepackage[active,tightpage,pdftex,floats]{preview}",
		"\\usepackage{booktabs}",
		"\\usepackage{caption}",
		"\\begin{document}",
		"\t\\newsavebox{\\tabularBoxgunfw}",  
		"\t\t\\sbox{\\tabularBoxgunfw}{",
		""
	)
	
	#MAGIC HAPPENS HERE
	#Assume 1 model for now
	model <- models[[1]]
	
	lines = c(lines,"\\begin{tabular}{lc}",
		"& \\textbf{Model 1}\\\\ \\toprule")
	var_list = variable.names(model)
	coef_list = model$coefficients
	p_list = coef(summary(model))[,4]
	N = nrow(model.frame(model))
	R2 = summary(model)$r.squared
	R2.p = lmp(model)
	ci = confint(model)
	
	for (i in 1:length(var_list)) {
		lines = c(lines, paste(c(
				var_list[i],
				" & ",
				ifelse(coef_list[i]>0,"\\phantom{$+$}",""),
				specify_decimal(coef_list[i],k)," ",
				ifelse(confint,paste(c("(",specify_decimal(ci[i,1],k),",",specify_decimal(ci[i,2],k),")"),collapse=""),""),
				ifelse(p_list[i]<.001,"***",ifelse(p_list[i]<.01, "**", ifelse(p_list[i]<.05,"*", ifelse(p_list[i]<.1,"^+","")))),"\\\\"),collapse=""))
	}
	lines = c(lines,"\\midrule")
	lines = c(lines,paste(c("\\textbf{$R^2$} &", specify_decimal(R2,k),"\\\\"), collapse=""),paste(c("\\textbf{N} &",N,"\\\\"), collapse=""),"\\end{tabular}")
	
	#Create end of file
	lines = c(lines, "","\t\t} %end hbox and savebox",
		"\t\\newlength{\\lengunfw}",
		"\t\\settowidth{\\lengunfw}{\\usebox{\\tabularBoxgunfw}}",
		"\\begin{table}",
		"\t\\begin{minipage}[h]{ \\lengunfw } \\usebox{ \\tabularBoxgunfw }",
		"\t\t\\begin{center}",
		"\t\t\t{ ***: $p<.001$, **: $p<.01$, *: $p<.05$, $^+$: $p<.1$}",
		"\t\t\\end{center}",
 		"\t\\end{minipage}\\end{table}",
 		"\\end{document}")	
	
	#save file
	out<-file(file)
	writeLines(lines,out)
	close(out)
}

