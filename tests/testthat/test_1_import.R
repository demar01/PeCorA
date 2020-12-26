test_that("import_LFQ_PeCorA throws error without valid input", {
  expect_error(import_LFQ_PeCorA("peptides_data",protein = "Leading.razor.protein",sequence= "Sequence",condition1="control",condition2="_COVID",condition3="NON.COVID"))
  expect_error(import_LFQ_PeCorA(peptides_data,protein = Leading.razor.protein,sequence= "Sequence",condition1="control",condition2="_COVID",condition3="NON.COVID"))
  expect_error(import_LFQ_PeCorA(peptides_data,protein = "Leading.razor.protein",sequence= Sequence,condition1="control",condition2="_COVID",condition3="NON.COVID"))
})
