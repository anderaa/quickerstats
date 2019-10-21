state_fips <- read.csv('scratch/state_fips.csv')
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


usethis::use_data(state_fips, internal = TRUE)
