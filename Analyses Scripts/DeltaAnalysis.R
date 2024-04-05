# Analyses for the Jobs/Economy and Housing/Roads Sections
# First, I set my working directory and loaded the below:
#setwd("")
library(tidyverse)
library(tidycensus)
library(dplyr)
library(stringr)

# Use the census API Key to access data
census_api_key("7c14e5ef3f0b07dee90eec286a5c4f75ff2fe0b9", install=TRUE)

# Counties in the Delta
delta_counties = c('Alameda', 'Contra Costa', 'Sacramento', 'San Joaquin', 'Solano', 'Yolo')

# Access Excel data 
delta_tracts = readxl::read_xlsx('finalctchris.xlsx')
delta_tracts_no_leading_zero = pull(delta_tracts[,2])

delta_tracts = delta_tracts %>%
  group_by(zone) %>%
  distinct()
getwd()
write_csv(delta_tracts, 'delta_tracts_no_duplicates.csv')

# Median Household Income:
# Obtain acs variable codes 
variables_delta <- load_variables(year = 2021, "acs5")
view(variables_delta)

# Retrieve median household income data for our census tracts
mhi_filtered <- get_acs(geography = "tract",
                                   variables = "B19019_001",
                                   year = 2021,
                                   state = "CA",
                                   counties = delta_counties,
)

# As Zach mentioned, we need to fix the missing leading zero
delta_tracts_total = delta_tracts_no_leading_zero

# Filter all the tracts for the ones in the CSV
delta_tracts_total = paste0(0, delta_tracts_total)
mhi_filtered = mhi_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

# Confirm that all census tracts are included
length(unique(delta_tracts_total))
length(unique(mhi_filtered$GEOID)) 

# Now we can find the median household income in the Delta
mhi_delta <- median(mhi_filtered$estimate, na.rm = TRUE)
print(mhi_delta)

# Mhi in Primary Zone
primary_zone_tracts <- delta_tracts %>%
  filter(zone == "1") %>%
  pull(tract)

primary_zone <- data.frame(tract = primary_zone_tracts)
primary_zone$tract <- as.character(primary_zone$tract)
primary_zone$tract <- str_pad(primary_zone$tract, width = 11, side = "left", pad = "0")

mhi_primary <- mhi_filtered %>%
  filter(GEOID %in% primary_zone$tract)
mhi_primary <- na.omit(mhi_primary)
mhi_primary <- median(mhi_primary$estimate, na.rm = TRUE)

print(mhi_primary)

# Mhi in Secondary Zone
secondary_zone_tracts <-delta_tracts %>%
  filter(zone == "2") %>%
  pull(tract)

secondary_zone <- data.frame(tract = secondary_zone_tracts)
secondary_zone$tract <- as.character(secondary_zone$tract)
secondary_zone$tract <- str_pad(secondary_zone$tract, width = 11, side = "left", pad = "0")

mhi_secondary <- mhi_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
mhi_secondary <- na.omit(mhi_secondary)
mhi_secondary <- median(mhi_secondary$estimate, na.rm = TRUE)

print(mhi_secondary)

# Mhi in California
mhi_california <- get_acs(
  geography = "state",
  variables = "B19019_001",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

mhi_california <- median(mhi_california$estimate, na.rm = TRUE)

print(mhi_california)

# Mhi in San Joaquin Valley
# Counties in the SJ Valley
sj_valley_counties <- c('Fresno', 'Kern', 'Kings', 'Madera', 'Merced', 'San Joaquin', 'Stanislaus', 'Tulare')

mhi_sj_valley <- get_acs(
  geography = "county",
  variables = "B19019_001",
  state = "CA",
  county = sj_valley_counties,
  survey = "acs5",
  year = 2021,
)

mhi_sj_valley <- median(mhi_sj_valley$estimate, na.rm = TRUE)

print(mhi_sj_valley)

# Unemployment Rates:
# Same process as before!
# Load variables for table DP03

dp03_vars <- load_variables(2016, "acs5/profile", cache = TRUE) %>%
       filter(str_detect(name, "DP03")) %>%
       pull(name)
 
dp03_us <- get_acs(geography = "us", 
                                                      variables = dp03_vars, 
                                                     year = 2021, 
                                                    output = "wide")

 
# Retrieve data for unemployment rate from table DP03
unemployment_filtered <- get_acs(geography = "tract",
                                 variables = "DP03_0009PE",
                                 year = 2021,
                                 state = "CA",
                                 counties = delta_counties)
 
unemployment_filtered = unemployment_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(unemployment_filtered$GEOID)) 

# Now we can find the unemployment rates in the Delta
unemployment_delta <- mean(unemployment_filtered$estimate, na.rm = TRUE)
print(unemployment_delta)

# Unemployment in Primary Zone

unemployment_primary <- unemployment_filtered %>%
  filter(GEOID %in% primary_zone$tract)
unemployment_primary <- na.omit(unemployment_primary)
unemployment_primary <- median(unemployment_primary$estimate, na.rm = TRUE)

print(unemployment_primary)

# Unemployment in Secondary Zone

unemployment_secondary <- unemployment_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
unemployment_secondary <- na.omit(unemployment_secondary)
unemployment_secondary <- mean(unemployment_secondary$estimate, na.rm = TRUE)

print(unemployment_secondary)

# Unemployment in California
unemployment_california <- get_acs(
  geography = "state",
  variables = "DP03_0009PE",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

unemployment_california <- mean(unemployment_california$estimate, na.rm = TRUE)

print(unemployment_california)

# Unemployment in San Joaquin Valley

unemployment_sj_valley <- get_acs(
  geography = "county",
  variables = "DP03_0009PE",
  state = "CA",
  county = sj_valley_counties,
  survey = "acs5",
  year = 2021,
)

unemployment_sj_valley <- mean(unemployment_sj_valley$estimate, na.rm = TRUE)

print(unemployment_sj_valley)

# Median Individual Earnings:
# Retrieve data for MIEs from table DP03
mie_filtered <- get_acs(geography = "tract",
                                 variables = "DP03_0092E",
                                 year = 2021,
                                 state = "CA",
                                 counties = delta_counties)

mie_filtered = mie_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(mie_filtered$GEOID)) 

# Now we can find the mies in the Delta
mie_delta <- median(mie_filtered$estimate, na.rm = TRUE)
print(mie_delta)

# MIE in Primary Zone

mie_primary <- mie_filtered %>%
  filter(GEOID %in% primary_zone$tract)
mie_primary <- na.omit(mie_primary)
mie_primary <- median(mie_primary$estimate, na.rm = TRUE)

print(mie_primary)

# MIE in Secondary Zone

mie_secondary <- mie_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
mie_secondary <- na.omit(mie_secondary)
mie_secondary <- median(mie_secondary$estimate, na.rm = TRUE)

print(mie_secondary)

# MIE in California
mie_california <- get_acs(
  geography = "state",
  variables = "DP03_0092E",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

mie_california <- median(mie_california$estimate, na.rm = TRUE)

print(mie_california)

# Average Commute Time:
# Code is: "DP03_0025E"

act_filtered <- get_acs(geography = "tract",
                        variables = "DP03_0025E",
                        year = 2021,
                        state = "CA",
                        counties = delta_counties)

act_filtered = act_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(act_filtered$GEOID)) 

# Now we can find the ACT in the Delta
act_delta <- mean(act_filtered$estimate, na.rm = TRUE)
print(act_delta)

# ACT in Primary Zone

act_primary <- act_filtered %>%
  filter(GEOID %in% primary_zone$tract)
act_primary <- na.omit(act_primary)
act_primary <- median(act_primary$estimate, na.rm = TRUE)

print(act_primary)

# ACT in Secondary Zone

act_secondary <- act_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
act_secondary <- na.omit(act_secondary)
act_secondary <- mean(act_secondary$estimate, na.rm = TRUE)

print(act_secondary)

# ACT in California
act_california <- get_acs(
  geography = "state",
  variables = "DP03_0025E",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

act_california <- mean(act_california$estimate, na.rm = TRUE)

print(act_california)

# Median Home Value
# This time it is for the DP04 Table
# Variable code: DP04_0089M
dp04_vars <- load_variables(2016, "acs5/profile", cache = TRUE) %>%
  filter(str_detect(name, "DP04")) %>%
  pull(name)

dp04_us <- get_acs(geography = "us", 
                   variables = "DP04_0077E", 
                   year = 2021, 
                   output = "wide")
dp04_us 

mhv_filtered <- get_acs(geography = "tract",
                        variables = "DP04_0089M",
                        year = 2021,
                        state = "CA",
                        counties = delta_counties)

mhv_filtered = mhv_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(mhv_filtered$GEOID)) 

# Now we can find the MHV in the Delta
mhv_delta <- median(mhv_filtered$estimate, na.rm = TRUE)
print(mhv_delta)

# MHV in Primary Zone

mhv_primary <- mhv_filtered %>%
  filter(GEOID %in% primary_zone$tract)
mhv_primary <- na.omit(mhv_primary)
mhv_primary <- median(mhv_primary$estimate, na.rm = TRUE)

print(mhv_primary)

# MHV in Secondary Zone

mhv_secondary <- mhv_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
mhv_secondary <- na.omit(mhv_secondary)
mhv_secondary <- median(mhv_secondary$estimate, na.rm = TRUE)

print(mhv_secondary)

# MHV in California
mhv_california <- get_acs(
  geography = "state",
  variables = "DP04_0089M",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

mhv_california <- median(mhv_california$estimate, na.rm = TRUE)

print(mhv_california)

# Owner-Occupied Homes vs. Renter-Occupied Homes
# Variable codes: DP04_0046PE (Owner) and DP04_0047PE (Renter)

#Owner-Occupied Check
ooh_filtered <- get_acs(geography = "tract",
                        variables = "DP04_0046PE",
                        year = 2021,
                        state = "CA",
                        counties = delta_counties)

ooh_filtered = ooh_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(ooh_filtered$GEOID)) 

#Renter-Occupied Check
roh_filtered <- get_acs(geography = "tract",
                        variables = "DP04_0047PE",
                        year = 2021,
                        state = "CA",
                        counties = delta_counties)

roh_filtered = roh_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(roh_filtered$GEOID)) 

# Now we can find the OOH and ROH in the Delta
ooh_delta <- mean(ooh_filtered$estimate, na.rm = TRUE)
print(ooh_delta)

roh_delta <- mean(roh_filtered$estimate, na.rm = TRUE)
print(roh_delta)

# OOH and ROH in Primary Zone

ooh_primary <- ooh_filtered %>%
  filter(GEOID %in% primary_zone$tract)
ooh_primary <- na.omit(ooh_primary)
ooh_primary <- mean(ooh_primary$estimate, na.rm = TRUE)

print(ooh_primary)

roh_primary <- roh_filtered %>%
  filter(GEOID %in% primary_zone$tract)
roh_primary <- na.omit(roh_primary)
roh_primary <- mean(roh_primary$estimate, na.rm = TRUE)

print(roh_primary)

# OOH and ROH in Secondary Zone

ooh_secondary <- ooh_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
ooh_secondary <- na.omit(ooh_secondary)
ooh_secondary <- mean(ooh_secondary$estimate, na.rm = TRUE)

print(ooh_secondary)

roh_secondary <- roh_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
roh_secondary <- na.omit(roh_secondary)
roh_secondary <- mean(roh_secondary$estimate, na.rm = TRUE)

print(roh_secondary)

# OOH and ROH in California
ooh_california <- get_acs(
  geography = "state",
  variables = "DP04_0046PE",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

ooh_california <- mean(ooh_california$estimate, na.rm = TRUE)

print(ooh_california)

roh_california <- get_acs(
  geography = "state",
  variables = "DP04_0047PE",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

roh_california <- mean(roh_california$estimate, na.rm = TRUE)

print(roh_california)

# OOH and ROH in the San Joaquin Valley

ooh_sj_valley <- get_acs(
  geography = "county",
  variables = "DP04_0046PE",
  state = "CA",
  county = sj_valley_counties,
  survey = "acs5",
  year = 2021,
)

ooh_sj_valley <- mean(ooh_sj_valley$estimate, na.rm = TRUE)

print(ooh_sj_valley)

roh_sj_valley <- get_acs(
  geography = "county",
  variables = "DP04_0047PE",
  state = "CA",
  county = sj_valley_counties,
  survey = "acs5",
  year = 2021,
)

roh_sj_valley <- mean(roh_sj_valley$estimate, na.rm = TRUE)

print(roh_sj_valley)

# Homeowner and Rental Vacancy Rates
# Variable Codes: DP04_0004E (Homeowner) DP04_0005E (Rental)

# Homeowner Vacancy Check
hv_filtered <- get_acs(geography = "tract",
                        variables = "DP04_0004E",
                        year = 2021,
                        state = "CA",
                        counties = delta_counties)

hv_filtered = hv_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(hv_filtered$GEOID)) 

# Rental Vacancy Check
rv_filtered <- get_acs(geography = "tract",
                        variables = "DP04_0005E",
                        year = 2021,
                        state = "CA",
                        counties = delta_counties)

rv_filtered = rv_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(rv_filtered$GEOID)) 

# Now we can find the HV and RV in the Delta
hv_delta <- mean(hv_filtered$estimate, na.rm = TRUE)
print(hv_delta)

rv_delta <- mean(rv_filtered$estimate, na.rm = TRUE)
print(rv_delta)

# HV and RV in Primary Zone

hv_primary <- hv_filtered %>%
  filter(GEOID %in% primary_zone$tract)
hv_primary <- na.omit(hv_primary)
hv_primary <- mean(hv_primary$estimate, na.rm = TRUE)

print(hv_primary)

rv_primary <- rv_filtered %>%
  filter(GEOID %in% primary_zone$tract)
rv_primary <- na.omit(rv_primary)
rv_primary <- mean(rv_primary$estimate, na.rm = TRUE)

print(rv_primary)

# HV and RV in Secondary Zone

hv_secondary <- hv_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
hv_secondary <- na.omit(hv_secondary)
hv_secondary <- mean(hv_secondary$estimate, na.rm = TRUE)

print(hv_secondary)

rv_secondary <- rv_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
rv_secondary <- na.omit(rv_secondary)
rv_secondary <- mean(rv_secondary$estimate, na.rm = TRUE)

print(rv_secondary)

# HV and RV in California
hv_california <- get_acs(
  geography = "state",
  variables = "DP04_0004E",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

hv_california <- mean(hv_california$estimate, na.rm = TRUE)

print(hv_california)

rv_california <- get_acs(
  geography = "state",
  variables = "DP04_0005E",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

rv_california <- mean(rv_california$estimate, na.rm = TRUE)

print(rv_california)

# Overcrowding Rates
# Variable Code: DP04_0079PE

or_filtered <- get_acs(geography = "tract",
                        variables = "DP04_0079PE",
                        year = 2021,
                        state = "CA",
                        counties = delta_counties)

or_filtered = or_filtered %>%
  filter(GEOID %in% delta_tracts_total) 

length(unique(delta_tracts_total))
length(unique(or_filtered$GEOID)) 

# Now we can find the Overcrowding Rates in the Delta
or_delta <- mean(or_filtered$estimate, na.rm = TRUE)
print(or_delta)

# Overcrowding Rates in Primary Zone

or_primary <- or_filtered %>%
  filter(GEOID %in% primary_zone$tract)
or_primary <- na.omit(or_primary)
or_primary <- mean(or_primary$estimate, na.rm = TRUE)

print(or_primary)

# Overcrowding Rates in Secondary Zone

or_secondary <- or_filtered %>%
  filter(GEOID %in% secondary_zone$tract)
or_secondary <- na.omit(or_secondary)
or_secondary <- mean(or_secondary$estimate, na.rm = TRUE)

print(or_secondary)

# Overcrowding Rates in California
or_california <- get_acs(
  geography = "state",
  variables = "DP04_0079PE",
  state = "CA",
  survey = "acs5",
  year = 2021,
)

or_california <- mean(or_california$estimate, na.rm = TRUE)

print(or_california)


# I created my graphs in Word because I am not that good at R!
# Road Conditions and Community Anchor Institution data were taken from ArcGIS Maps.
# Please see the maps uploaded to the GitHub for that data :)