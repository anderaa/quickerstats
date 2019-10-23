
# This script creates the data sets written to R/sysdata.rda

# organize and write state data
state_fips <- read.csv('scratch/state_fips.csv', stringsAsFactors = FALSE)
state_fips['state_fips_string'] <- NA
for (i in 1:nrow(state_fips)) {
  if (state_fips[i, 'state_fips'] < 10) {
    state_fips[i, 'state_fips_string'] <- paste('0', as.character(state_fips[i, 'state_fips']), sep='')
  } else {
    state_fips[i, 'state_fips_string'] <- as.character(state_fips[i, 'state_fips'])
  }
}
state_fips <- state_fips[c('name', 'state_fips_string')]
colnames(state_fips) <- c('state', 'state_fips')


# organize and write county data
county_fips <- read.csv('scratch/county_fips.csv', stringsAsFactors = FALSE)
county_fips['county_fips'] <- NA
county_fips['county_code_string'] <- NA
county_fips['state_fips_string'] <- NA
county_fips['state'] <- NA
for (i in 1:nrow(county_fips)) {
  # fix state code string
  if (county_fips[i, 'state_fips'] < 10) {
    county_fips[i, 'state_fips_string'] <- paste('0', as.character(county_fips[i, 'state_fips']), sep='')
  } else {
    county_fips[i, 'state_fips_string'] <- as.character(county_fips[i, 'state_fips'])
  }
  # fix county code string
  if (county_fips[i, 'county_code'] < 10) {
    county_fips[i, 'county_code_string'] <- paste('00', as.character(county_fips[i, 'county_code']), sep='')
  } else if ((county_fips[i, 'county_code'] < 100)) {
    county_fips[i, 'county_code_string'] <- paste('0', as.character(county_fips[i, 'county_code']), sep='')
  } else {
    county_fips[i, 'county_code_string'] <- as.character(county_fips[i, 'county_code'])
  }
  # paste state and county together
  county_fips[i, 'county_fips'] <- paste(county_fips[i, 'state_fips_string'],
                                         county_fips[i, 'county_code_string'],
                                         sep='')
  # add in state names
  county_fips[i, 'state'] <- state_fips[state_fips$state_fips == county_fips[i, 'state_fips_string'], 'state']

}
county_fips <- county_fips[c('name', 'state', 'county_fips')]
colnames(county_fips) <- c('county', 'state', 'county_fips')

usethis::use_data(county_fips, state_fips, internal = TRUE, overwrite=TRUE)
