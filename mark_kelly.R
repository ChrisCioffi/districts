#Load packages
library(tidyverse)
library(tidyusafec)
library(tigris)
library(sf)
library(leaflet)
library(censusapi)

#load API keys from env file
save_datagov_apikey(key = "n3BB27dCbHpsI0BAIyYi5i4nMa3xJk9AXF7cG2Hc")
census_key = Sys.getenv("6ca1427d5d740735f295c8fc411c95119b1100c9")

#tell tigris we're working with shape files
options(tigris_class = "sf")

## create a function that does the opposite of `%in%`
`%notin%` <- Negate(`%in%`)


#pull the data we want from the API
rawContribs <- search_candidates(name = c("KELLY, MARK"), 
                           election_year = "2020", 
                           office ="S",
                           candidate_status = "C",
                           has_raised_funds = TRUE, 
                           unnest_committees = TRUE) %>% 
  get_itemized_contributions(data_structure = "tidy") %>% 
  unnest_wider(committee, names_repair = "unique") %>% 
  select(schedule_type_full, 
         report_type, 
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
filter(report_year > 2018 & line_number %in% "11AI" & memo_code %notin% "X" )


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
                        key = "6ca1427d5d740735f295c8fc411c95119b1100c9")

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

# download arizona sf data from tigris
az_sf <- zctas(cb = FALSE, year = 2010, state = "AZ")

# join that sf data with normalized_az df
map_data <- geo_join(az_sf, az_totals, "ZCTA5CE10", "ZCTA")

#and there's a few counties that have a total of 0 (their populations are a total of 27k residents), so we're going to replace those values with zeroes.We'll create a new column just in case
map_data <- map_data %>% 
  mutate(total_raised_no_na = replace_na(total_raised, 0))

## trying to export for qgis in desperation
write_csv(map_data, "output/az_map_data.csv")

ggplot(az_sf) +
  geom_sf()


#TODO: learn how to do a spatial subset in order to have the state boundary of arizona as well as the zcta boundaries 
## Also figure out how to get the colors right and maybe also to speed this up
## or just give it up and figure out how to turn to qgis
arizona_map <- ggplot(map_data) +
  geom_sf(data = map_data) +
  aes(fill= map_data$total_raised) +
  geom_sf(color="black")  +
  # scale_fill_manual(values = c( "#C9C5DB", "#938CB8", "#3E386D")) +
  theme_void() +
  labs(title="Mark Kelly's contributions by zcta per 100", color='legend', fill='legend title')
arizona_map


# write_csv(map_data, "output/az_map_data.csv") ## trying to export for qgis


bins <- c(0, 1, 3, 6, 10, 31, Inf)
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
