library(stringr)

test_that("generates with all parameters TRUE", {
	fileName.target <- './tests/testthat/lmer_all.tex'
	target <- readChar(fileName.target, file.info(fileName.target)$size)
	
	model1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
	
	lmer_to_tex("./tests/testthat/test.tex",list(model1))
	
	fileName.compare <- './tests/testthat/test.tex'
	comparison <- readChar(fileName.compare, file.info(fileName.compare)$size)
	
	#target generated on windows. Just in case running on *nix, remove \r.
	target <- str_replace_all(target, "[\r]", "")
	comparison <- str_replace_all(comparison, "[\r]", "")
	
	expect_equal(target, comparison)
})

test_that("generate with all parameters FALSE", {
	fileName.target <- './tests/testthat/lmer_missing.tex'
	target <- readChar(fileName.target, file.info(fileName.target)$size)
	
	model1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
	
	lmer_to_tex("output.tex",list(model1), confint=FALSE, fit=FALSE, sample.size=FALSE)
	
	fileName.compare <- './tests/testthat/test.tex'
	comparison <- readChar(fileName.compare, file.info(fileName.compare)$size)
	
	#target generated on windows. Just in case running on *nix, remove \r.
	target <- str_replace_all(target, "[\r]", "")
	comparison <- str_replace_all(comparison, "[\r]", "")
	
	expect_equal(target, comparison)
})

test_that("Test that 3 decimals fail", {
	fileName.target <- './tests/testthat/lmer_all.tex'
	target <- readChar(fileName.target, file.info(fileName.target)$size)
	
	model1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
	
	lmer_to_tex("./tests/testthat/test.tex",list(model1), decimals = 3)
	
	fileName.compare <- './tests/testthat/test.tex'
	comparison <- readChar(fileName.compare, file.info(fileName.compare)$size)
	
	#target generated on windows. Just in case running on *nix, remove \r.
	target <- str_replace_all(target, "[\r]", "")
	comparison <- str_replace_all(comparison, "[\r]", "")
	
	expect_false(target==comparison)
})

