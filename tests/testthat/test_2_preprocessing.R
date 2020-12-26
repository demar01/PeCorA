test_that("PeCorA_preprocessing throws error without valid input", {
  expect_error(PeCorA_preprocessing("pecora_format",area_column_name = 4 , threshold_to_filter= 100, control_name= "control"))
  expect_error(PeCorA_preprocessing(pecora_format,area_column_name = "4" , threshold_to_filter= 100, control_name= "control"))
  expect_error(PeCorA_preprocessing(pecora_format,area_column_name = 4 , threshold_to_filter= "100", control_name= "control"))
  expect_error(PeCorA_preprocessing(pecora_format,area_column_name = 4 , threshold_to_filter= 100, control_name= control))

})
