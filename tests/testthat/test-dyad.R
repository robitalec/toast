context("test-dyad")

# Read example data
DT <- fread(system.file("extdata", "DT.csv", package = "toast"))


test_that("cols are checked", {
	expect_error(
		dyad_id(DT, focal = 'potato', neighbour = NULL),
		'focal column not found in DT'
	)

	copyDT <- copy(DT)[, ID := ID == 'A']
	expect_error(
		dyad_id(copyDT, 'ID'),
		'does not match required type', fixed = FALSE
	)
})
