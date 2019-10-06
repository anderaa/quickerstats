
# Aaron Anderson, USDA/NWRC
# See https://github.com/anderaa/quicker-stats


library(httr)
library(readr)


########################################################################################################################
get_param_values <- function(param) {
  # Return all values parameter can take. See bottom of script for list of paramters.
  
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
search_data_items <- function(search_terms, exclude=c()) {
  # Get available data items based on search terms.
  # Args:
  #   search_terms: A vector of search terms. Each result will include all terms.
  #   exclude: A vector of search terms to exclude. No result will have any of these.
  # Returns:
  #   A list of all search results.
  
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
get_county_item_count <- function(key, year, data_item, fips='all', domain='TOTAL') {
  # Get the count of values that exist for the specified query.
  # Args:
  #   key: Your NASS API key.
  #   year: Must be a census year (e.g. 2012, 2017).
  #   data_item: The long description of the desired series. Use search_data_items function to find one.
  #   fips: Must be 'all', a 2-digit state fips, or a 5-digit county fips.
  #   domain: A modifier on data_item, some characterstic (e.g. size categories of operations), use 'all' to get all.
  # Returns:
  #   The count.
  
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
get_county_data <- function(key, year, data_item, fips='all', domain='TOTAL') {
  # Get the data for the specified query.
  # Args:
  #   key: Your NASS API key.
  #   year: Must be a census year (e.g. 2012, 2017).
  #   data_item: The long description of the desired series. Use search_data_items function to find one.
  #   fips: Must be 'all', a 2-digit state fips, or a 5-digit county fips.
  #   domain: A modifier on data_item, some characterstic (e.g. size categories of operations), use 'all' to get all.
  # Returns:
  #   A tibble df of the requested data, if any exists. Otherwise returns NA.
  
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
get_state_item_count <- function(key, year, data_item, fips='all', domain='TOTAL') {
  # Get the count of values that exist for the specified query.
  # Args:
  #   key: Your NASS API key.
  #   year: Must be a census year (e.g. 2012, 2017)
  #   data_item: The long description of the desired series. Use search_data_items function to find one.
  #   fips: Must be 'all' or a 2-digit state fips
  # Returns:
  #   The count.
  
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
get_state_data <- function(key, year, data_item, fips='all', domain='TOTAL') {
  # Get the data for the specified query.
  # Args:
  #   key: Your NASS API key.
  #   year: Must be a census year (e.g. 2012, 2017)
  #   data_item: The long description of the desired series. Use search_data_items function to find one.
  #   fips: Must be 'all' or a 2-digit state fips
  # Returns:
  #   A tibble df of the requested data, if any exists. Otherwise returns NA.
  
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


# key <- 'D8EECA1B-A77F-3520-9CBE-9E25548F10E4'
# 
# search_data_items(search_terms=c('corn', 'harvested'), exclude=c('sweet'))
# search_data_items(search_terms=c('corn', 'price'), exclude=c('sweet'))
# 
# get_county_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
# get_county_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
# 
# get_county_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
# get_county_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
# 
# get_county_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08069', domain='all')
# get_county_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08069', domain='all')
# 
# get_state_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
# get_state_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
# 
# get_state_item_count(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
# get_state_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')


# API documentation here: https://quickstats.nass.usda.gov/api
#
# "What" Parameters:
#   source_desc (Program): Source of data
#   sector_desc (Sector): Broad categories to narrow choices 
#   group_desc (Group): Subgroups with sector_desc
#   commodity_desc (Commodity): Primary subject of interest
#   class_desc: Generally a physical attribute of the commodity
#   prodn_practice_desc: A method of production or action taken
#   util_practice_desc: Utilization or marketing channel
#   statisticcat_desc (Category): The aspect of the commodity being measured
#   unit_desc: The unit associated with statisticcat_desc
#   short_desc (Data Item): Concat of of commodity_desc, class_desc, prodn_practice_desc, 
#                util_practice_desc, statisticcat_desc, unit_desc
#   domain_desc (Domain): Another characteristic of operations that produce similar commodities
#   domaincat_desc: Categories or partitions within a domain
#
# "Where" Parameters:
#   agg_level_desc (Geographic Level): Geographic granularity of data  
#   state_ansi: 2-digit ansi state code
#   state_fips_code: 2-digit fips state code
#   state_alpha: State abbreviation
#   state_name (State): Full name of state
#   asd_code: NASS agg district code
#   asd_desc (Ag District): NASS ag district name
#   county_ansi: 3-digit ansi county code
#   county_code: 3-digit NASS county code, matches ansi for lower 48
#   county_name (County): County name
#   region_desc (Region): NASS defined geo entity
#   zip_5 (Zip Code): 5-digit zip code
#   watershed_code: USGS hydrologic code for watershed
#   watershed_desc (Watershed): USGS watershed name
#   congr_district_code: US congressional district 2-digit code
#   country_code: 4-digit country code
#   country_name: Country name
#   location_desc: Full description for the location dimension
#   
# "When" Parameters
#   year (Year): 4-digit year
#   freq_desc (Period Type): Length of time covered
#   begin_code: 2-digit code corresponding to start of reference period
#   end_code: 2-digit code corresponding to end of reference period
#   reference_period_desc (Period): Time frame withing freq_desc
#   week_ending: Week ending date
#   load_time: Datetime of entry into quickstats db

