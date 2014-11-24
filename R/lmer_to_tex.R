#Note, helper functions are in helper.R

#' lmer model to TeX
#'
#' This function takes a list of lme4::lmer models and outputs TeX code to create a pretty table.
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
#' @param confint Logical if \code{TRUE} adds confidence interval to the table.
#' @param fit Logical if \code{TRUE} adds fit information (R^2) to the table.
#' @param sample.size Logical if \code{TRUE} adds sample size to the table.
#' @param decimals int Number of decimal places.
#' @return NULL. Just outputs the table as a file.
#' @note We approximate p values using lmerTest::summary
#' @import lme4
#' @import lmerTest
#' @export
#' @examples
#' ## Example from lme4::lmer:
#' ## linear mixed models - reference values from older code
#' model1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' model2 <- lmer(Reaction ~ Days + I(Days^2) + (Days | Subject), sleepstudy)
#' 
#' lmer_to_tex("output.tex",list(model1, model2))
#' lmer_to_tex("output.tex",list(model1), confint=FALSE, fit=FALSE, sample.size=FALSE)
lmer_to_tex <- function(file, models, confint=T, fit=T, sample.size=T, groups=T, decimals=2) {
	require(lme4)
	require(lmerTest)
	
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
	summ <- summary(model)
	var_list = row.names(summ$coefficients)
	coef_list = summ$coefficients
	p_list = coef(summary(model))[,5]
	N = nrow(model.frame(model))
	N.groups = summ$ngrps[[1]]
	AIC = AIC(logLik(model))
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
	lines = c(lines,paste(c("\\textbf{AIC} &", specify_decimal(AIC,k),"\\\\"), collapse=""),paste(c("\\textbf{Obs.} &",N,"\\\\"), collapse=""),paste(c("\\textbf{Groups} &",N.groups,"\\\\"), collapse=""),"\\end{tabular}")
	
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

