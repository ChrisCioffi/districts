#Load packages
library(tidyverse)
library(tidyusafec)
library(tigris)
library(sf)
library(leaflet)
library(censusapi)

#load API keys from env file
save_datagov_apikey(key = Sys.getenv("FEC_API_KEY"))
census_key = Sys.getenv("CENSUS_API_KEY")

#tell tigris we're working with shape files
options(tigris_class = "sf")

## create a function that does the opposite of `%in%`
`%notin%` <- Negate(`%in%`)



#pull the data we want from the API  #these fields filters ensure we're pulling recent results, they're not clouded up with winred/actblue passthroughs and they're definitely from individuals
rawContribs <- search_candidates(name = c("KELLY, MARK", "MCSALLY, MARTHA" ), 
                                 # search_candidates(name = c("KELLY, MARK"), 
                                 #search_candidates(name = c("MCSALLY, MARTHA"),
                           election_year = "2020", 
                           office ="S",
                           candidate_status = "C",
                           has_raised_funds = TRUE, 
                           unnest_committees = TRUE) %>% 
  get_itemized_contributions(data_structure = "tidy") %>% 
  unnest_wider(committee, names_repair = "unique") %>% 
  select(schedule_type_full, 
         report_type, 
         entity_type,
         line_number, 
         line_number_label, 
         contributor_state, 
         is_individual, 
         report_year, 
         contributor_city, 
         contribution_receipt_date, 
         contributor_aggregate_ytd, 
         two_year_transaction_period, 
         contribution_receipt_amount, 
         contributor_zip, 
         contributor_name, 
         state, 
         party, 
         committee_type_full, 
         state_full, 
         name, 
         fec_election_type_desc, 
         memo_code) %>%
filter(report_year > 2018 & line_number %in% "11AI" & memo_code %notin% "X" & is_individual == TRUE & entity_type %in% "IND")


# create a dataframe of in-state contributions
instate_funds <- rawContribs %>% 
  select(report_type, 
         contributor_name,
         contributor_city,
         contributor_state,
         contributor_zip,
         contribution_receipt_amount,
         contribution_receipt_date) %>% 
  filter(contributor_state=="AZ")
#preserve zeroes of zipcodes, then chop last five numbers
instate_funds$contributor_zip <- as.character(instate_funds$contributor_zip)
instate_funds$contributor_zip <- substr(instate_funds$contributor_zip, 1, 5)


###############################      NOTE      #######################################
##                                                                                  ##
##       there are three 00000 zipcodes, here are the images for them:              ##
##       Pio, J https://docquery.fec.gov/cgi-bin/fecimg/?201910159164670517         ##
##       Judy, Todd https://docquery.fec.gov/cgi-bin/fecimg/?201904159146387245     ##
##       Swift, Susan https://docquery.fec.gov/cgi-bin/fecimg/?201910159164670481   ##
##                                                                                  ##
######################################################################################  

# import zcta crosswalk csv, then join on zip codes
# link for reference https://www.udsmapper.org/zcta-crosswalk.cfm
crosswalk <- read_csv("data/zip_to_zcta_2019.csv")
crosswalked_contribs <- left_join(instate_funds, crosswalk, by=c("contributor_zip" = "ZIP_CODE"))
  

## run these to learn about the census api
# geography <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "geography")
# variables <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "variables")
# apis <- listCensusApis()
# call population data from the census (this calls population for all zctas)
population <- getCensus(name = "acs/acs5",
                        vintage = 2018,
                        vars = c("B01003_001E", "GEO_ID"),
                        region = "zip code tabulation area:*",
                        key = census_key)

#rename population data column population$B01003_001E to "TOTAL_POPULATION"
population <- rename(population, 
                     total_population=B01003_001E) 

# create a new dataframe by joining population data to instate_funds data, dropping any zips that don't match.
az_zcta <- left_join(crosswalked_contribs, population, by=c("ZCTA" = "zip_code_tabulation_area"))

#normalize the totals column by population, then multiply by a number that seems to make sense for the population
### NOTE: this dropped zips 85341 and 85726 with population of 0, totaling $1135 in contributions across five donors.
### These addresses were listed as PO Boxes on receipts.
#### Normalization by 100  people also creates a situation where zip 86003 w/ pop. of 3 has given nearly 37K, creating a big outlier, next nearest 1K
normalized_az <- mutate(az_zcta,
                        normalized_total=(contribution_receipt_amount/total_population)*100)

#get the total raised per zcta
az_totals <- normalized_az %>% 
  group_by(ZCTA) %>% 
  summarise(total_raised = sum(normalized_total))

# write_csv(az_totals, "output/az_totals_yearend.csv")

#round normalized_total to two decimals
az_totals <- az_totals %>% 
  mutate(total_raised=round(total_raised, digits=2))

#removes the two Inf total raised, which are the zips that were listed above ... apparently they didn't get dropped before after all?
az_totals <- az_totals %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

write_csv(az_totals, "output/mark_kelly_normalized_zcta.csv")

# download arizona sf data from tigris
az_sf <- zctas(cb = FALSE, year = 2010, state = "AZ")

# join that sf data with normalized_az df
map_data <- geo_join(az_sf, az_totals, "ZCTA5CE10", "ZCTA")

#and there's a few counties that have a total of 0 (their populations are a total of 27k residents), so we're going to replace those values with zeroes.We'll create a new column just in case
map_data <- map_data %>% 
  mutate(total_raised_no_na = replace_na(total_raised, 0))



bins <- c(0, 25, 50, 150, 250, Inf)
pal1 <- colorBin(palette = c("#FFFFFF", "#C9C5DB","#05B69C", "#F9A51A", "#C73D49"), domain = map_data$total_raised_no_na, bins = bins)
map <- leaflet(map_data) %>% addTiles()
state_popup1 <- paste0("<strong> Zip: </strong>", 
                       map_data$ZCTA5CE10, 
                       "<br><strong>Total Raised: </strong>", 
                       map_data$total_raised_no_na)
leaflet(data = map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal1(total_raised_no_na), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup1) %>%
  addLegend("bottomright", pal = pal1, values = ~total_raised_no_na,
            title = "Total raised",
            labFormat = labelFormat(prefix = " "))


#TODO: pull state by state population data -- (state contributions/state populations)/100k



#TODO: export out-of-state data for bar chart via graphics rig. this only requires top ten states and totals



#### Stashing leaflet code here for further work this weekend
#leaflet works much faster and it gives us a more interactive graphic, which i'm partial to.

bins <- c(0, 100, 1000, 10000, 100000, 200000, Inf)
pal1 <- colorBin(palette = c("#FFFFFF", "#C9C5DB","#05B69C", "#F9A51A", "#C73D49"), domain = cong_tot$total_raised_no_na, bins = bins) 
map <- leaflet(cong_tot) %>% addTiles()
state_popup1 <- paste0("<strong> District: </strong>", 
                       cong_tot$ZCTA5CE10, 
                       "<br><strong>Total Raised: </strong>", 
                       cong_tot$total_raised_no_na)
leaflet(data = cong_tot) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal1(total_raised_no_na), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup1) %>%
  addLegend("bottomright", pal = pal1, values = ~total_raised_no_na,
            title = "Total raised",
            labFormat = labelFormat(prefix = " "))



###############################      NOTE      #######################################
##                                                                                  ##
##                                  Queries for analysis                            ##
##                                                                                  ##
######################################################################################  


#gets total number of money by state
by_state <- rawContribs %>%
  group_by(contributor_state, name) %>%
  summarise(total_raised = sum(contribution_receipt_amount))

#gets count of contributors from each state and city
by_contribs <- rawContribs %>%
  group_by(contributor_state, contributor_city, name) %>%
  summarise(count = n(contributor_name))

#gets sum of total money raised by city
by_city <- rawContribs %>%
  group_by(contributor_city, name) %>%
  summarise(total_raised = sum(contribution_receipt_amount))

#sums the amount of money by date
by_date <- rawContribs %>%
  group_by(contribution_receipt_date, name) %>%
  summarise(total_raised = sum(contribution_receipt_amount))

#sums the amount of money by quarter
by_quarter_money <- rawContribs %>%
  group_by(report_type, name) %>%
  summarise(total_raised = sum(contribution_receipt_amount))

by_quarter_count <- rawContribs %>%
  group_by(report_type, name) %>%
  summarise(count = n())
