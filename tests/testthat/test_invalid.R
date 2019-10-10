
context("Errors from invalid queries")

test_that("check for error with bad key", {
  expect_error(get_param_values(key='bad_key', param='short_desc'))
  expect_error(get_options(key='bad_key',
                           data_item='CORN, GRAIN - ACRES HARVESTED'))
  expect_error(search_data_items(key='bad_key',
                                 search_terms=c('corn')))
  expect_error(get_state_data(key='bad_key', year=2017,
                              data_item='CORN, GRAIN - ACRES HARVESTED',
                              fips='all'))
  expect_error(get_county_data(key='bad_key', year=2017,
                               data_item='CORN, GRAIN - ACRES HARVESTED',
                               fips='all'))
})
