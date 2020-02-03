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

#create a dataframe of out-of-state contributions
outstate_funds <- rawContribs %>%
  select(report_type,
         contributor_name,
         contributor_city,
         contributor_state,
         contributor_zip,
         contribution_receipt_amount,
         contribution_receipt_date) %>%
  filter(contributor_state != "AZ")
#preserve zeroes of zipcodes, then chop last five numbers
  outstate_funds$contributor_zip <- as.character(outstate_funds$contributor_zip)
  outstate_funds$contributor_zip <- substr(outstate_funds$contributor_zip, 1, 5)

state_totals <- outstate_funds %>%
  group_by(contributor_state) %>%
  summarise(total_raised = sum(contribution_receipt_amount))

write_csv(state_totals, "output/kelly_outofstate.csv")
