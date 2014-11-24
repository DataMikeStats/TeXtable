#Note, helper functions are in helper.R

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
#'
#' @param file chr file name for output.
#' @param models List of lm models.
#' @param confint Logical if \code{TRUE} (default) adds confidence interval to the table.
#' @param fit Logical if \code{TRUE} (default) adds fit information (R^2) to the table.
#' @param sample.size Logical if \code{TRUE}  (default) adds sample size to the table.
#' @param decimals int Number of decimal places (2 by default).
#' @return NULL. Just outputs the table as a file.
#' @export
#' @examples
#' ## Example from lm:
#' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' model1 <- lm(weight ~ group)
#' model2 <- lm(weight ~ group - 1) # omitting intercept
#' 
#' lm_to_tex("output.tex",list(model1, model2))
#' lm_to_tex("output.tex",list(model1), confint=FALSE, fit=FALSE, sample.size=FALSE)
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
	summ <- summary(model)
	lines = c(lines,"\\begin{tabular}{lc}",
		"& \\textbf{Model 1}\\\\ \\toprule")
	var_list = variable.names(model)
	coef_list = model$coefficients
	p_list = coef(summ)[,ncol(coef(summ))] #Assumes p value is in the last column
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
	lines = c(lines,paste(c("\\textbf{$R^2$} &", specify_decimal(R2,k), ifelse(R2.p<.001,"***",ifelse(R2.p<.01, "**", ifelse(R2.p<.05,"*", ifelse(R2.p<.1,"^+","")))),"\\\\"), collapse=""),paste(c("\\textbf{N} &",N,"\\\\"), collapse=""),"\\end{tabular}")
	
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

