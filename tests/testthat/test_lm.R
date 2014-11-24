library(stringr)

test_that("generates with all parameters TRUE", {
	fileName.target <- './tests/testthat/lm_all.tex'
	target <- readChar(fileName.target, file.info(fileName.target)$size)
	
	ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
	trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
	group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
	weight <- c(ctl, trt)
	model1 <- lm(weight ~ group)
	
	lm_to_tex("./tests/testthat/test.tex",list(model1))
	
	fileName.compare <- './tests/testthat/test.tex'
	comparison <- readChar(fileName.compare, file.info(fileName.compare)$size)
	
	#target generated on windows. Just in case running on *nix, remove \r.
	target <- str_replace_all(target, "[\r]", "")
	comparison <- str_replace_all(comparison, "[\r]", "")
	
	expect_equal(target, comparison)
})

test_that("generate with all parameters FALSE", {
	fileName.target <- './tests/testthat/lm_missing.tex'
	target <- readChar(fileName.target, file.info(fileName.target)$size)
	
	ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
	trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
	group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
	weight <- c(ctl, trt)
	model1 <- lm(weight ~ group)
	
	lm_to_tex("./tests/testthat/test.tex",list(model1), confint=FALSE, fit=FALSE, sample.size=FALSE)
	
	fileName.compare <- './tests/testthat/test.tex'
	comparison <- readChar(fileName.compare, file.info(fileName.compare)$size)
	
	#target generated on windows. Just in case running on *nix, remove \r.
	target <- str_replace_all(target, "[\r]", "")
	comparison <- str_replace_all(comparison, "[\r]", "")
	
	expect_equal(target, comparison)
})

test_that("Test that 3 decimals fail", {
	fileName.target <- './tests/testthat/lm_all.tex'
	target <- readChar(fileName.target, file.info(fileName.target)$size)
	
	ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
	trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
	group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
	weight <- c(ctl, trt)
	model1 <- lm(weight ~ group)
	
	lm_to_tex("./tests/testthat/test.tex",list(model1), decimals = 3)
	
	fileName.compare <- './tests/testthat/test.tex'
	comparison <- readChar(fileName.compare, file.info(fileName.compare)$size)
	
	#target generated on windows. Just in case running on *nix, remove \r.
	target <- str_replace_all(target, "[\r]", "")
	comparison <- str_replace_all(comparison, "[\r]", "")
	
	expect_false(target==comparison)
})

