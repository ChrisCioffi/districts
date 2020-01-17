#Access data from the openFEC api using R wrapper from Stephen Holzman 
#https://stephenholzman.github.io/tidyusafec/articles/intro.html

library(tidyverse)
library(tidyusafec)
library(zipcode)
library(noncensus)
library(tigris)
library(leaflet)

#signup for api key at https://api.open.fec.gov/developers/. Save a one line file called "data.gov.key" in the root project folder, that one line assigning the key to a variable like the next line:
save_datagov_apikey(key = "n3BB27dCbHpsI0BAIyYi5i4nMa3xJk9AXF7cG2Hc")

#select all candidates running for Senate, unnest the data, deliver it in a df, and make sure they raised money 
#For North Carolina races
#senate <- search_candidates(state = "NC", election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>%
#for alaska races (this is useful because the file is somewhat small..... See my sampling script at the bottom for a way to take a radom sample to make testing much speedier)
senate <- search_candidates(state = "AK", election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>%
#for colorado races
#senate <- search_candidates(state = "CO", election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>%
#for arizona races
#senate <- search_candidates(state = "AZ", election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>% 
  #get all their itemized contributions
  get_itemized_contributions(data_structure = "tidy") %>%
  #unnests the pesky committee column and creates unique columns for each of the nested list items.
  #here's a great tutorial on how to get rid of nested lists https://cfss.uchicago.edu/notes/simplify-nested-lists/ Thanks Andrew Tran for pointing me to this
  #something else he suggested. I didn't need it, but am putting it here for safe keeping https://jennybc.github.io/purrr-tutorial/ls00_inspect-explore.html
  unnest_wider(committee, names_repair = "unique") %>%
  #select only the fields I want.
  select(schedule_type_full, report_type, line_number, line_number_label, contributor_state, is_individual, report_year, contributor_city, contribution_receipt_date, contributor_aggregate_ytd, two_year_transaction_period, contribution_receipt_amount, contributor_zip, contributor_name, state, party, committee_type_full, state_full, name, fec_election_type_desc) %>%
  filter(report_year > 2018 & line_number %in% "11AI")


#Rural/urban codes were downloaded from https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx
#fec data accessed from https://classic.fec.gov/disclosurep/PDownload.do
#subsets the first 5 numbers of the zipcodes row https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/substr
senate$contributor_zip <- substr(senate$contributor_zip, 1, 5)
#FYI, if you want the last 5, use https://www.rdocumentation.org/packages/FedData/versions/1.1.0/topics/substrRight

#write_csv(senate, "NC_Senate.csv")
write_csv(senate, "AK_Senate.csv")
#write_csv(senate, "CO_Senate.csv")
#write_csv(senate, "AZ_Senate.csv")


#let's see what and where these senators are getting their funding. This query is just for Tillis donations... and I need to go back and make sure there's no pacs in it. Right now it's set up just to make the map work.
totals <- senate %>%
  #this filter changes based on the race
  #filter(name %in% "THOM TILLIS COMMITTEE") %>%
  filter(name %in% "ALASKANS FOR DAN SULLIVAN") %>%
  group_by(contributor_zip) %>%
  summarise( total_raised = sum(contribution_receipt_amount))


#to create the file, depending on what you want to find
#write_csv(totals, "NC_totals.csv")
#write_csv(totals, "AK_totals.csv")
#write_csv(totals, "Co_totals.csv")
#write_csv(totals, "AZ_totals.csv")

##########
#returns tigris query file as a shapefile (if not, it's a weird list that doesn't join)
options(tigris_class = "sf")

#Download a Zip Code Tabulation Area (ZCTA) shapefile into R
#read in our shapefile

options(tigris_use_cache = TRUE)
#code_shapefile <- zctas(cb = FALSE, year = 2010, state = "NC")
code_shapefile <- zctas(cb = FALSE, year = 2010, state = "AK")
#code_shapefile <- zctas(cb = FALSE, year = 2010, state = "CO")
#code_shapefile <- zctas(cb = FALSE, year = 2010, state = "AZ")

#join democratic fundraising numbers with shapefile
#make sure the zip column is numeric and then join the two, so we can make the shapefile and contributions data work together like happy friends in leaflet
cong_tot <- left_join(code_shapefile, totals , by = c("ZCTA5CE10"= "contributor_zip" ))

#########works, but is so slow because of all the shapefiles to plot.

#ggplot(cong_tot) + 
#  geom_sf(data = cong_tot) +
#  aes(fill= cut(total_raised,breaks = c(-1, 10000, 30000, 60000, 120000), labels= c("0-9k", "10k-29k", "30k-59k", "60k-120k") )) + 
#  geom_sf(color="black")  +
#  scale_fill_manual(values = c("#FFFFFF", "#C9C5DB", "#938CB8", "#3E386D")) +
#  theme_void() + 
#  labs(title="Funds raised by Thom Tillis in NC zip codes", caption="Source: Federal Elections Commission", color='legend', fill='legend title')


#leaflet works much faster and it gives us a more interactive graphic, which i'm partial to.

bins <- c(0, 100, 500, 1000, 5000, 10000, Inf)
pal1 <- colorBin("inferno", domain = cong_tot$total_raised, bins = bins)
map <- leaflet(cong_tot) %>% addTiles()
state_popup1 <- paste0("<strong> District: </strong>", 
                       cong_tot$contributor_zip, 
                       "<br><strong>Total Raised: </strong>", 
                       cong_tot$total_raised)
leaflet(data = cong_tot) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal1(total_raised), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup1) %>%
  addLegend("bottomright", pal = pal1, values = ~total_raised,
            title = "Total raised",
            labFormat = labelFormat(prefix = " "))





#helpful for running code in a smaller subset of data for testing purposes
dt <- 
  #df %>%
  #slices off number of rows from 1:xx in df
  #slice(1:4)
  #slices off random number of rows from data
  sample_n(senate, 10)


#from zip code package. Was used in original code. And Supposedly cleans up zip codes. Cran has been rudely pulled down the orphan package which I found rude. It's useful. But if it's gone, it's gone. 
#senate$contributor_zip <- clean.zipcodes(senate$contributor_zip)
#conjures up the zip_codes table from the noncensus library. Which is a great resource for this project. Because, unlike the zip codes data(zipcodes) table. It has fips codes.
#data(zip_codes)
#however, there was a lack of zeroes in the fips code fields. And since the clean.zipcodes puts zeroes in front of some four-digit I'm using it here to match up the Census database.
#zip_codes$fips <- clean.zipcodes(zip_codes$fips)



#############This will be useful if we need to group by county. Not just by zip. Until then, it shall be noted and sit idle.

#counties <- read_csv("ZIP_COUNTY.csv")
#props to Dhmontgomery in newsnerdery. to helping me eliminate the excess dupes by using a combination of rank and filter...basically what top_n does. But with the added ability of adding the ties.method element. #I was able to select the highest ratios and then, in the case of ties, R randomly chose one. 
#nodupes <- counties %>% 
#  group_by(zip) %>%
#  mutate(rank = rank(tot_ratio, ties.method = "random")) %>% 
#  filter(rank < 2)

######################
