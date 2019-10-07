

########################################################################################################################
#' Get all values a parameter can take.
#'
#' Get all values of a parameters that can be passed in a GET request.
#' See \url{https://quickstats.nass.usda.gov/api} for a table of parameter names.
#'
#' @param param The parameter name.
#' @return A list of all values that the parameter can take.
#' @examples
#' get_param_values('source_desc')
#' get_param_values('domain_desc')
get_param_values <- function(param) {
  param <- 'short_desc'
  url <- paste('http://quickstats.nass.usda.gov/api/get_param_values/?',
               'key=', key,
               '&param=', param,
               sep='')
  r <- GET(url)
  return(content(r))
}
########################################################################################################################

########################################################################################################################
#' Get available data items based on search terms.
#'
#' @param search_terms A vector of search terms. Each result will include all terms.
#' @param exclude A vector of search terms to exclude. No result will have any of these.
#' @return A list of all search results.
#' @examples
#' search_data_items(search_terms=c('corn', 'harvested'), exclude=c('sweet'))
#' search_data_items(search_terms=c('corn', 'price'), exclude=c())
search_data_items <- function(search_terms, exclude=c()) {
  items <- get_param_values(param='short_desc')
  results <- c()
  for (i in 1:length(items[[1]])) {
    # check for any exclude terms
    skip <- FALSE
    for (term in exclude) {
      if (grepl(toupper(term), items[[1]][[i]][[1]], fixed=TRUE)) {
        skip <- TRUE
        break
      }
    }
    # if any exclude terms were found, skip rest of loop
    if (skip) {
      next
    }
    # check for all search terms
    matches <- 0
    for (term in search_terms) {
      if (grepl(toupper(term), items[[1]][[i]][[1]], fixed=TRUE)) {
        matches <- matches + 1
      }
    }
    # if all search terms were present, add data item to results
    if (matches == length(search_terms)) {
      results <- c(results, items[[1]][[i]][[1]])
    }
  }

  return(results)
}
########################################################################################################################

########################################################################################################################
#' Get the count of values that exist for the specified query for county-level data.
#'
#' @param key Your NASS API key.
#' @param year Must be a census year (e.g. 2012, 2017).
#' @param data_item The long description of the desired series. Use search_data_items function to find one.
#' @param fips Must be 'all', a 2-digit state fips, or a 5-digit county fips.
#' @param domain A modifier on data_item, some characterstic (e.g. size categories of operations), use 'all' to get all.
#' @return The count of values.
#' @examples
#' get_county_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
#' get_county_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
#' get_county_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08069', domain='all')
get_county_item_count <- function(key, year, data_item, fips='all', domain='TOTAL') {
  base_url <- paste('http://quickstats.nass.usda.gov/api/get_counts/?',
                    'key=', key,
                    '&short_desc=', data_item,
                    '&year__GE=', year,
                    '&agg_level_desc=COUNTY',
                    '&source_desc=CENSUS',
                    sep='')

  # add specific domain if needed
  if (domain != 'all') {
    base_url <- paste(base_url, '&domain_desc=', domain, sep='')
  }

  # get all counties in US
  if (fips == 'all') {
    url <- base_url
  }
  # get all counties in a state
  else if (nchar(fips) == 2) {
    url <- paste(base_url, '&state_fips_code=', fips, sep='')
  }
  # get a specific county
  else if (nchar(fips) == 5) {
    state_fips <- substr(fips, 1, 2)
    county_fips <- substr(fips, 3, 5)
    url <- paste(base_url, '&state_fips_code=', state_fips, '&county_ansi=', county_fips, sep='')
  }
  else {
    print('The fips argument must be "all" or a 2-digit state fips or a 5-digit county fips')
    return(NA)
  }
  # make the request
  r <- GET(url)
  return(content(r)$count)
}
########################################################################################################################

########################################################################################################################
#' Get the data for the specified query for county-level data.
#'
#' @param key Your NASS API key.
#' @param year Must be a census year (e.g. 2012, 2017).
#' @param data_item The long description of the desired series. Use search_data_items function to find one.
#' @param fips Must be 'all', a 2-digit state fips, or a 5-digit county fips.
#' @param domain A modifier on data_item, some characterstic (e.g. size categories of operations), use 'all' to get all.
#' @return A tibble df of the requested data, if any exists. Otherwise returns NA.
#' @examples
#' get_county_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
#' get_county_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
#' get_county_ddta(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08069', domain='all')
get_county_data <- function(key, year, data_item, fips='all', domain='TOTAL') {
  # check if any data exists
  if (get_county_item_count(key, year, data_item, fips, domain) == 0) {
    print('No data exists for this particular query. Try modifying query paramters.')
    return(NA)
  }

  base_url <- paste('http://quickstats.nass.usda.gov/api/api_GET/?',
                    'key=', key,
                    '&short_desc=', data_item,
                    '&year__GE=', year,
                    '&agg_level_desc=COUNTY',
                    '&source_desc=CENSUS',
                    '&format=CSV',
                    sep='')

  # add specific domain if needed
  if (domain != 'all') {
    base_url <- paste(base_url, '&domain_desc=', domain, sep='')
  }

  # get all counties in US
  if (fips == 'all') {
    url <- base_url
  }
  # get all counties in a state
  else if (nchar(fips) == 2) {
    url <- paste(base_url, '&state_fips_code=', fips, sep='')
  }
  # get a specific county
  else if (nchar(fips) == 5) {
    state_fips <- substr(fips, 1, 2)
    county_fips <- substr(fips, 3, 5)
    url <- paste(base_url, '&state_fips_code=', state_fips, '&county_ansi=', county_fips, sep='')
  }
  else {
    print('The fips argument must be "all" or a 2-digit state fips or a 5-digit county fips')
    return(NA)
  }
  # make the request
  r <- GET(url)
  return(content(r))
}
########################################################################################################################

########################################################################################################################
#' Get the count of values that exist for the specified query for state-level data.
#'
#' @param key Your NASS API key.
#' @param year Must be a census year (e.g. 2012, 2017).
#' @param data_item The long description of the desired series. Use search_data_items function to find one.
#' @param fips Must be 'all' or a 2-digit state fips.
#' @param domain A modifier on data_item, some characterstic (e.g. size categories of operations), use 'all' to get all.
#' @return The count of values.
#' @examples
#' get_state_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
#' get_state_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
get_state_item_count <- function(key, year, data_item, fips='all', domain='TOTAL') {
  base_url <- paste('http://quickstats.nass.usda.gov/api/get_counts/?',
                    'key=', key,
                    '&short_desc=', data_item,
                    '&year__GE=', year,
                    '&agg_level_desc=STATE',
                    '&source_desc=CENSUS',
                    sep='')

  # add specific domain if needed
  if (domain != 'all') {
    base_url <- paste(base_url, '&domain_desc=', domain, sep='')
  }

  # get all counties in US
  if (fips == 'all') {
    url <- base_url
  }
  # get all counties in a state
  else if (nchar(fips) == 2) {
    url <- paste(base_url, '&state_fips_code=', fips, sep='')
  }
  else {
    print('The fips argument must be "all" or a 2-digit state fips')
    return(NA)
  }
  # make the request
  r <- GET(url)
  return(content(r)$count)
}
########################################################################################################################

########################################################################################################################
#' Get the data for the specified query for state-level data.
#'
#' @param key Your NASS API key.
#' @param year Must be a census year (e.g. 2012, 2017).
#' @param data_item The long description of the desired series. Use search_data_items function to find one.
#' @param fips Must be 'all' or a 2-digit state fips.
#' @param domain A modifier on data_item, some characterstic (e.g. size categories of operations), use 'all' to get all.
#' @return A tibble df of the requested data, if any exists. Otherwise returns NA.
#' @examples
#' get_state_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
#' get_state_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
get_state_data <- function(key, year, data_item, fips='all', domain='TOTAL') {
  # check if any data exists
  if (get_state_item_count(key, year, data_item, fips, domain) == 0) {
    print('No data exists for this particular query. Try modifying query paramters.')
    return(NA)
  }

  base_url <- paste('http://quickstats.nass.usda.gov/api/api_GET/?',
                    'key=', key,
                    '&short_desc=', data_item,
                    '&year__GE=', year,
                    '&agg_level_desc=STATE',
                    '&source_desc=CENSUS',
                    '&format=CSV',
                    sep='')

  # add specific domain if needed
  if (domain != 'all') {
    base_url <- paste(base_url, '&domain_desc=', domain, sep='')
  }

  # get all counties in US
  if (fips == 'all') {
    url <- base_url
  }
  # get all counties in a state
  else if (nchar(fips) == 2) {
    url <- paste(base_url, '&state_fips_code=', fips, sep='')
  }
  else {
    stop('The fips argument must be "all" or a 2-digit state fips')
  }
  # make the request
  r <- GET(url)
  return(content(r))
}
########################################################################################################################

# TODO: delete these lines
# key <- 'D8EECA1B-A77F-3520-9CBE-9E25548F10E4'
# devtools::document()
# ?get_param_values
# ?search_data_items
# ?get_county_item_count
# ?get_county_data
# ?get_state_item_count
# ?get_state_data
