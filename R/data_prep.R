## Note this file is not required for replication, it is used to create the anonymised data 
## before running this code it is necessary to set the d.path route to the local data file which contains full dataset
# d.path should point the analysis folder and the working directory should be set (in data_prep_local.R)


##----read-local-data----
library(tidyverse)
library(here)

imain<-read.csv(file.path(a.path, "MP_votes_cleaned.csv"), stringsAsFactors = FALSE)

new.anon<-read.csv(file.path(a.path, "anon_email_responses2.csv"), stringsAsFactors = FALSE)
add_autoresponses <- read.csv(file.path(a.path, "anon_email_autoresponses.csv"), stringsAsFactors = FALSE)

add_autoresponses <- add_autoresponses %>% 
  filter(!is.na(letterid)) %>% 
  group_by(letterid) %>% 
  summarise(autoresponse = sum(autoresponse), autoresponse_exists=max(autoresponse_exists))
new.anon <- left_join(new.anon, add_autoresponses, by="letterid") 
new.anon2 <- new.anon %>%
  mutate(autoresponse_exists = ifelse(is.na(autoresponse_exists), 0, 1))


text.summary<-read_csv(paste0(d.path, "/BA_MP_Experiment/data sources/text_analysis.csv"))

mp_rebel_dat <- read_csv(paste0(d.path, "/BA_MP_Experiment/data sources/mp_rebellion_YOS_updated.csv"))

bes.dat<-read.csv(file.path(a.path, "BES-2015-General-Election-results-file-v2.2.csv"), stringsAsFactors = FALSE)
bes17.dat<-haven::read_sav(paste0(d.path, "/BA_MP_Experiment/data sources/BES-2017-General-Election-results-file-v1.0.sav"))

##----parlparsedata----

con_url <- "https://raw.githubusercontent.com/mysociety/parlparse/refs/heads/master/people/constituencies.json"
#people_list <- fromJSON(url, simplifyVector = FALSE)
con_list <- fromJSON(con_url)

# Load people JSON
people_url <- "https://raw.githubusercontent.com/mysociety/parlparse/refs/heads/master/members/people.json"
people_list <- fromJSON(people_url)

# The below gets the two (nested) data.frames which we can clean in regular ways (so we no longer need to worry about the JSON format)
memberships <- people_list$memberships |> as_tibble()
persons <- people_list$persons |> as_tibble()
posts <- people_list$posts

### Handle awkward details in posts
MP_posts <- posts |>
  as_tibble() |>
  filter(organization_id == "house-of-commons",
         role =="Member of Parliament") |>
  unnest_wider(area) |>
  rename(post_id = id,
         constituency_name = name)
 
##----parlparsepersonnames----


# Cleaning the person names data - which is another of the nested columns in the persons tibble
# We probably want one name per person - rather than all their names - but not sure which name to select - probably a name with the following as priority in order:
# 1. a name with both a family name and a given name
# 2. at the time of the experiment active 'main' (as opposed to 'alternative') name 
# 3. We also have information on the original ordering of the names, which we can use to break ties 
# Date of experiment 14-11-2018 to 3-7-2019

person_names <- persons |>
  select(id, other_names) |>
  unnest(other_names) |>
  mutate(init_name_order = row_number()) |>
  ungroup() |>
  mutate(
    is_current_order = case_when(
      (is.na(start_date)|ymd(start_date)<ymd("2018-11-14")) & (is.na(end_date)|ymd(end_date)>ymd("2018-11-14")) ~ 1,
      TRUE ~ 2
    ),
    both_names_order = case_when(
      !is.na(family_name)&!is.na(given_name) ~ 1,
      TRUE ~ 2
    ),
    is_main_order = case_when(
      note == "Main" ~ 1,
      TRUE ~ 2
    )) |> 
  group_by(id) |>
  arrange(id, both_names_order, is_current_order, is_main_order, init_name_order) |> # Find main name as main name current at start of experiment: 
  slice(1)

## NOTE: there seem to be 47 person records that have no associated names, not sure what/whom these records refer to 
anti_join(persons, person_names, by = "id") |> nrow()

## NOTE: there are also 65 cases that don't have any identifiers (65 cases of 14629 total), and these will be dropped by this code - likely this doesn't apply to any recent MPs - a quick check of five cases of those dropped by this procedure suggests that these are generally people who have not been MPs (e.g. The Queen, a former member of the London Assembly). All 47 of the cases above which don't have name information also do not have identifier information
anti_join(persons, identifiers_wide, by = "id") |> nrow()

# This is the cleaned flat dataset with identifiers and names information
persons_wide <- persons |> 
  select(-identifiers, -other_names) |>
  left_join(identifiers_wide, by = "id") |>
  left_join(person_names, by = "id")

##----parlparselengthofservice----

memberships_subset <- memberships %>%
  inner_join(MP_posts |> select(-start_date, -end_date), by = "post_id") |>
  select(person_id, start_date, end_date, post_id)

# write.csv(memberships_subset, "memberships.csv", row.names = FALSE)
# memberships <- read.csv("memberships.csv", stringsAsFactors = FALSE)

# Convert end_date to Date if not already
memberships_subset <- memberships_subset %>%
  # filter(!organization_id %in% c("crown", "house-of-lords", "london-assembly")) |>
  mutate(
    start_date = ymd(start_date),
    end_date = ymd(end_date)
  )

# Define the target window
window_start <- ymd("2017-06-09")
window_end <- ymd("2019-07-01")

# Step 1: Filter MPs whose membership overlaps with that window
memberships_window <- memberships_subset %>%
  filter(start_date <= window_end & end_date >= window_start)

# Step 2: Get the full membership records for those MPs
relevant_mps <- memberships_subset %>%
  filter(person_id %in% memberships_window$person_id) |>
  left_join(MP_posts |> select(constituency_name, post_id))


# Step 3: Get the minimum start_date for each of those MPs
mps_min_start <- relevant_mps %>%
  group_by(person_id) %>%
  summarise(first_start = min(start_date, na.rm = TRUE)) %>%
  ungroup() 

# Step 4: Link to constituency names (with same names as in the BES data)
# posts data has the same constituency names as the BES except for a minor difference for Carmarthen West and Pembrokshire South
# However, there are duplicate entries in posts for MP who change parties or became independent - we want one link to constituency for each MP
mps_min_start <- mps_min_start|>
  left_join(memberships_window |> select(person_id, post_id)) |>
  left_join(MP_posts |> select(post_id, constituency_name)) |>
  select(-post_id) |>
  group_by(person_id, first_start, constituency_name) |>
  summarise() |>
  ungroup() |>
  mutate(ConstituencyName = case_when(
    constituency_name == "Carmarthen West and South Pembrokeshire" ~ "Carmarthen West and Pembrokeshire South",
    TRUE ~ constituency_name
  )) |>
  left_join(person_names |> select(id, family_name, given_name), by = c("person_id"="id"))

# mps_min_start|>
#   left_join(person_names |> select(id, family_name, given_name), by = c("person_id"="id")) |> 
#   arrange(first_start)
# 
# con_matches <- mps_min_start$constituency_name[!mps_min_start$constituency_name %in% bes17.dat$ConstituencyName]
# con_matches <- bes17.dat$ConstituencyName[!bes17.dat$ConstituencyName %in% mps_min_start$constituency_name]

##----parlparsefrontbench----

# 1) Download ministerial roles (2010–present)
min_json <- "https://raw.githubusercontent.com/mysociety/parlparse/master/members/ministers-2010.json"
ministers <- fromJSON(min_json, flatten = TRUE)

# 2) Converting to tibbles
roles <- ministers$memberships |> as_tibble()

# 3) Clean IDs and dates
roles <- roles %>%
  transmute(
    # person_id = str_extract(person_id, "\\d+"),
    person_id = person_id,
    role = role,
    start_date = as.Date(start_date),
    end_date   = as.Date(ifelse(is.na(end_date) | end_date == "", "9999-12-31", end_date))
  )

# 4) Experiment window
window_start <- as.Date("2018-11-14")
window_end   <- as.Date("2019-07-03")

# 5) Define “minister or shadow minister”
pattern <- regex("Secretary|Minister|Shadow", ignore_case = TRUE)

# 6) Filter to matching roles that overlap the window
frontbenchers <- roles %>%
  filter(
    !is.na(start_date),
    str_detect(role, pattern),
    start_date <= window_end,
    end_date >= window_start
  ) 

# 6) Count unique MPs
n_ministers <- n_distinct(frontbenchers$person_id)
n_ministers

mps_min_start_frontbench <- mps_min_start |>
  mutate(frontbench = person_id %in% frontbenchers$person_id
  )
bes17.dat <- bes17.dat |>
  left_join(mps_min_start_frontbench |> 
              rename(mp_first_start = first_start), by = "ConstituencyName")
##----process-local-data----

new.anon <- new.anon %>%
  dplyr::select(letterid, constituency, issue, position, send_position, manualresponse_exists, multiresponse, id, manualresponse_exists_inc_post, respondent_reported_letter_status, autoresponse_exists) %>%
  mutate(senderid=id) %>%
  mutate(manualresponse_exists_inc_post_count_nonresponse = ifelse(is.na(manualresponse_exists_inc_post), 0, manualresponse_exists_inc_post),
         autoresponse_exists = ifelse(is.na(autoresponse_exists), 0, autoresponse_exists),
         response_auto_and_manual = autoresponse_exists|manualresponse_exists_inc_post_count_nonresponse)


## letterid fixer
# holding letters to be replaced:
text.summary <- text.summary %>% 
  filter(!letterid %in% c(386, 387, 296, 386, 387, 384, 52, 251, 401, 400, 2009, 1058, 2026))

# letterid 444 is from ** to ** goes to letterid 384

fixlid <- function(x){
  fixliddf<-tribble(
    ~wrongletterid, ~letterid,
    341,     296,
    433,     386,
    442,     387,
    444,     384,
    450,     52,
    471,     251,
    512,     401,
    514,     400,
    589,     2009,
    805,      1058,
    829,     2026
  )
  if(x %in% fixliddf$wrongletterid){
    y <- fixliddf$letterid[fixliddf$wrongletterid==x]
  } else {
    y<-x
  }
  return(y)
}

text.summary <- text.summary %>% 
  rowwise %>%
  mutate(letterid = fixlid(letterid))

### Select text analysis for main letters only
mainletters <- text.summary %>% filter(withinid==1) %>%group_by(letterid) %>% slice(1)

### Assign zeros to NA text measures

mainletters <- mainletters %>%
  mutate(
    across(starts_with("sum"), ~replace_na(.x, 0)),
    across(starts_with("word"), ~replace_na(.x, 0))
  )

### Merge into main data by letterid

new.anon <- left_join(new.anon, mainletters, by="letterid")



#Create party positions

# customs: After leaving the EU, there should be a customs union between the UK and the EU.
# singlem: After leaving the EU, the UK should stay in the European Single Market.
# freedomm: After leaving the EU, the UK should accept Freedom of Movement with the EU
# secondr: There should be a second referendum on the UK's decision to leave the European Union.
# regulation: After leaving the EU, the UK should remove regulations related to the EU's protection of workers' rights, human rights, environmental, health and safety standards.
# immigration: The UK should impose much tougher controls on immigration and reduce drastically the number of immigrants coming to Britain.
# skilled: The UK's immigration policy should focus on allowing more skilled immigrants to come and work in the country and significantly reduce the level of unskilled immigration.
# tuitionlevel: The level of university tuition fees in the UK should vary across subject.
# tuitionfree: The government should abolish university tuition fees.

# table(main$party)
# table(main$issue)

con_pos<-data.frame(party="Con", 
                    immigration = 1, 
                    customs = 0, 
                    singlem = 0, 
                    freedomm = 0, 
                    secondr = 0,
                    regulation = 1,
                    tuitionfree = 0,
                    tuitionlevel = 1,
                    skilled = 1
)
lab_pos<-data.frame(party="Lab", 
                    immigration = 0, 
                    customs = 1, 
                    singlem = 0, 
                    freedomm = 0, 
                    secondr = 0,
                    regulation = 0,
                    tuitionfree = 1,
                    tuitionlevel = 0,
                    skilled = 0
)

LD_pos<-data.frame(party="ldem", 
                   immigration = 0, 
                   customs = 1, 
                   singlem = 1, 
                   freedomm = 1, 
                   secondr = 1,
                   regulation = 0,
                   tuitionfree = 0,
                   tuitionlevel = 0,
                   skilled = 0
)

green_pos<-data.frame(party="Green", 
                      immigration = 0, 
                      customs = 1, 
                      singlem = 1, 
                      freedomm = 1, 
                      secondr = 1,
                      regulation = 0,
                      tuitionfree = 1,
                      tuitionlevel = 0,
                      skilled = 0
)

party_pos <- bind_rows(con_pos, lab_pos, LD_pos, green_pos)%>%
  tidyr::gather(issue, partyposition, -party) %>%
  mutate(party=tolower(party))

mp_rebel_dat <- rename(mp_rebel_dat, "constituency_name"="constituency", "issue_red"="issue")

# main <- left_join(new.anon, imain, by=c("constituency"="onsconstid"))
main <- new.anon %>% as_tibble()

# get rid of non-party MPs

main <- filter(main, constituency!="E14000760")
names(main)<-tolower(names(main))
main$onsconstid<-main$constituency

issue_expander <- data.frame(issue=c("customs", "freedomm", "immigration", "regulation", "secondr", "singlem", "tuitionfree", "tuitionlevel", "skilled"), issue_red = c("customs", "freedomm", "immigration", "regulation", "secondr", "singlem", "tuitionfree", "tuitionfree", "immigration"))

main <- left_join(main, issue_expander, by="issue") %>% as_tibble()



mp_rebel_summary <- mp_rebel_dat %>%
  mutate(rebellion_strict = ifelse(is.na(rebellion_strict), 0, rebellion_strict),
         rebellion_strict_abstain_nonreb = ifelse(vote == "absent", 0, rebellion_strict),
         rebellion_strict_abstain_nonreb = ifelse(is.na(rebellion_strict_abstain_nonreb), 0, rebellion_strict_abstain_nonreb),
         rebellion_weak = ifelse(is.na(rebellion_weak), 0, rebellion_weak),
         rebellion_strict_issue_explicit = ifelse(rebellion_strict == 1 & rebellion_on_issue_check == 1 & !is.na(rebellion_on_issue_check), 1, 0),
         rebellion_weak_issue_explicit = ifelse(rebellion_weak == 1 & rebellion_on_issue_check == 1  & !is.na(rebellion_on_issue_check), 1, 0),
         rebellion_weak_direction = case_when(
           rebellion_weak == 1 & rebellion_weak_issue_explicit == 1 &
             rebellion_direction == "Moderate" ~ "Moderate rebellion", 
           rebellion_weak == 1 & rebellion_weak_issue_explicit == 1 &
             rebellion_direction == "Extreme" ~ "Extreme rebellion",
           rebellion_weak == 1 & rebellion_weak_issue_explicit == 0  ~ 
             "Unclear rebellion",
           rebellion_weak == 1 & is.na(rebellion_direction)  ~ 
             "Unclear rebellion", 
           rebellion_weak == 0 ~ "No rebellion", 
           TRUE ~ "NA"
         ),
         rebellion_strict_direction = case_when(
           rebellion_strict == 1 & rebellion_strict_issue_explicit == 1 &
             rebellion_direction == "Moderate" ~ "Moderate rebellion", 
           rebellion_strict == 1 & rebellion_strict_issue_explicit == 1 &
             rebellion_direction == "Extreme" ~ "Extreme rebellion",
           rebellion_strict == 1 & rebellion_strict_issue_explicit == 0  ~ 
             "Unclear rebellion",
           rebellion_strict == 1 & is.na(rebellion_direction)  ~ 
             "Unclear rebellion", 
           rebellion_strict == 0 ~ "No rebellion", 
           TRUE ~ "NA"
         )) %>%
  group_by(ONSConstID, constituency_name, mp_name, issue_red, party.full) %>%
  summarize(rebellion_strict=max(rebellion_strict, na.rm=TRUE),
            rebellion_strict_abstain_nonreb=max(rebellion_strict_abstain_nonreb, na.rm=TRUE),
            rebellion_strict_issue_explicit=max(rebellion_strict_issue_explicit, na.rm=TRUE),
            rebellion_strict_moderate=as.numeric(any(rebellion_strict_direction == "Moderate rebellion", na.rm=TRUE)),
            rebellion_strict_extreme=as.numeric(any(rebellion_strict_direction == "Extreme rebellion", na.rm=TRUE)),
            rebellion_weak=max(rebellion_weak, na.rm=TRUE),
            rebellion_weak_issue_explicit=max(rebellion_weak_issue_explicit, na.rm=TRUE),
            rebellion_weak_moderate=as.numeric(any(rebellion_weak_direction == "Moderate rebellion", na.rm=TRUE)),
            rebellion_weak_extreme=as.numeric(any(rebellion_weak_direction == "Extreme rebellion", na.rm=TRUE))
  ) %>%
  mutate(party=tolower(party.full))

main <- left_join(main, mp_rebel_summary, by=c("onsconstid" ="ONSConstID", "issue_red")) %>% as_tibble()


#Create party positions

# customs: After leaving the EU, there should be a customs union between the UK and the EU.
# singlem: After leaving the EU, the UK should stay in the European Single Market.
# freedomm: After leaving the EU, the UK should accept Freedom of Movement with the EU
# secondr: There should be a second referendum on the UK's decision to leave the European Union.
# regulation: After leaving the EU, the UK should remove regulations related to the EU's protection of workers' rights, human rights, environmental, health and safety standards.
# immigration: The UK should impose much tougher controls on immigration and reduce drastically the number of immigrants coming to Britain.
# skilled: The UK's immigration policy should focus on allowing more skilled immigrants to come and work in the country and significantly reduce the level of unskilled immigration.
# tuitionlevel: The level of university tuition fees in the UK should vary across subject.
# tuitionfree: The government should abolish university tuition fees.

# table(main$party)
# table(main$issue)

con_pos<-data.frame(party="Con", 
                    immigration = 1, 
                    customs = 0, 
                    singlem = 0, 
                    freedomm = 0, 
                    secondr = 0,
                    regulation = 1,
                    tuitionfree = 0,
                    tuitionlevel = 1,
                    skilled = 1
)
lab_pos<-data.frame(party="Lab", 
                    immigration = 0, 
                    customs = 1, 
                    singlem = 0, 
                    freedomm = 0, 
                    secondr = 0,
                    regulation = 0,
                    tuitionfree = 1,
                    tuitionlevel = 0,
                    skilled = 0
)

LD_pos<-data.frame(party="ldem", 
                   immigration = 0, 
                   customs = 1, 
                   singlem = 1, 
                   freedomm = 1, 
                   secondr = 1,
                   regulation = 0,
                   tuitionfree = 0,
                   tuitionlevel = 0,
                   skilled = 0
)

green_pos<-data.frame(party="Green", 
                      immigration = 0, 
                      customs = 1, 
                      singlem = 1, 
                      freedomm = 1, 
                      secondr = 1,
                      regulation = 0,
                      tuitionfree = 1,
                      tuitionlevel = 0,
                      skilled = 0
)

party_pos <- bind_rows(con_pos, lab_pos, LD_pos, green_pos)%>%
  tidyr::gather(issue, partyposition, -party) %>%
  mutate(party=tolower(party))

main <- left_join(main, party_pos, by=c("party", "issue"))

letter_pos_numeric<-function(x){
  ifelse(x=="agree", 1, 0)
}

main <- main %>%
  mutate(positionn = letter_pos_numeric(position),
         partycongruent=as.numeric(positionn == partyposition),
         mp_type = ifelse(rebellion_strict, "rebel", "loyalist"),
         mp_type_detailed = case_when(
           rebellion_strict == 0 ~ "loyalist",
           rebellion_strict == 1 & rebellion_strict_moderate == 1 & rebellion_strict_extreme == 0  ~ "moderate rebel",
           rebellion_strict == 1 & rebellion_strict_moderate == 0 & rebellion_strict_extreme == 1  ~ "extreme rebel",
           rebellion_strict == 1 & rebellion_strict_moderate == 1 & rebellion_strict_extreme == 1 ~ "bothways rebel",
           rebellion_strict == 1 & rebellion_strict_issue_explicit == 0 ~ "unclear rebel",
           TRUE ~ "NA"
         ),
         mp_type_abstain_nonreb = ifelse(rebellion_strict_abstain_nonreb, "rebel", "loyalist"))

## Finalise text measures of responses

main <- main %>%
  mutate(sum_lab_lab_mp = replace(sum_lab, party != "lab", 0), 
         sum_con_con_mp = replace(sum_con, party != "con", 0), 
         sum_lib_lib_mp = replace(sum_lib, party != "ldem", 0),
         sum_gre_gre_mp = replace(sum_gre, party != "green", 0),
         sum_party_mentions = sum_we + sum_lab_lab_mp + sum_con_con_mp + sum_lib_lib_mp + sum_gre_gre_mp,
         lr_mp_party = log((1+sum_i)/(1+sum_party_mentions)),
         diff_mp_party = (sum_i / word_total) - (sum_party_mentions / word_total),
         diff_mp_party_outliers_removed = case_when(
           diff_mp_party %in% boxplot.stats(diff_mp_party)$out ~ NA_real_,
           TRUE ~ as.numeric(diff_mp_party)
         )
  )

main <- main %>% 
  mutate(send_position_red = case_when(
    send_position == 1 ~ "1",
    send_position %in% c(2:6) ~ "2-6",
    TRUE ~ "beyond 6"
  ))           


# Design: map/balance test population vs. sample in experiment [GC]
# Do we want to do this at the constituency level?
#   Ingredients?
#   BES 2017 constituency results file
# Indicator of whether at least two letters are sent  for each constitunecy
# Characteristics to compare?
#   Prop. MP Party (Lab/Con/Oth)
# Marginality
# Region (how to code?)
# Average leave/remain vote
# Education level (mean % degree)

bes17.dat <- bes17.dat %>% 
  mutate(in_experimentn = ifelse(ONSConstID %in% main$constituency, "Included", "Excluded"))%>%
  mutate(in_experiment = ifelse(ONSConstID %in% main$constituency, "Included (letters sent)", "Excluded (no letters)"))

# bes17.dat %>% 
#   group_by(in_experiment) %>%
#   summarise(
#     "Lab MP" = scales::percent(mean(Winner17==2), accuracy = .1),
#     "Con MP" = scales::percent(mean(Winner17==1), accuracy = .1),
#     "Oth MP" = scales::percent(mean(!Winner17 %in% c(1,2)), accuracy = .1),
#     "Av.leave" = scales::percent(mean(leaveHanretty)/100, accuracy = .1),
#     "Av.majority" = scales::percent(mean(Majority17)/100, accuracy = .1),
#     "Av.degree" = scales::percent(mean(c11Degree, na.rm=TRUE)/100, accuracy =.1)
#   ) %>%
#   rename(" " = in_experiment) %>%
#   huxtable()%>% 
#   # set_background_color(evens, everywhere, "grey95") %>%
#   set_bold(row = 1, col = everywhere) %>% 
#   set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
#   set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
#   set_caption("Characteristics of constituencies included in and excluded from the experiment.")

main <- main %>% 
  mutate(
    orig_send_position = send_position,
    send_cat = case_when(
      send_position == 1 ~ "1",
      send_position %in% c(2:6) ~ "2-6",
      TRUE ~ "beyond 6"
    ),
    emailonlyresponse = case_when(
      respondent_reported_letter_status==1 ~ 0,
      manualresponse_exists==1 ~ 1,
      TRUE ~ NA_real_
    ),
    has_response_info = as.numeric(!is.na(respondent_reported_letter_status)),
    partycongruent_neat = recode(partycongruent, 
                                 `0` = "Constituent-party not congruent",
                                 `1` = "Constituent-party congruent")
  )



issue_nice <- tribble(
  ~issue, ~issue_nice,
  "customs", "Support EU/UK Customs Union",
  "singlem", "Support remaining in single market",
  "freedomm", "Support EU/UK Freedom of movement",
  "secondr", "Support second referendum",
  "regulation", "Support removing EU regulations",
  "immigration", "Support tougher immigration control",
  "skilled", "Support points based immigration system",
  "tuitionfree", "Support abolition of university tuition fees",
  "tuitionlevel", "Support variable tuition fees across subjects"
)
issue_nice$issue_order <- 1:nrow(issue_nice)

const_with_multi_mps <-mp_rebel_dat %>% 
  group_by(ONSConstID, constituency_name, mp_name)%>%
  summarize() %>%
  group_by(ONSConstID, constituency_name) %>% 
  tally() %>%
  filter(n>1)

n_byelection_affected<-sum(new.anon$constituency %in% const_with_multi_mps$ONSConstID)



## ----save-party-shield-processed-data-local----
root_dir <- here::here()
data_dir <- file.path(root_dir, "Data")
current_dir <- file.path(root_dir, "R")

set.seed(123)
main_ids <- main |>
  group_by(onsconstid) |>
  summarise(msender = max(senderid)) |>
  arrange(msender) |>
  rowid_to_column("const_anon_id")
  
main_small <- main |>
  left_join(main_ids, by = "onsconstid") |>
  dplyr::select(
    id,
    const_anon_id,
    response_auto_and_manual,
    manualresponse_exists_inc_post_count_nonresponse,
    manualresponse_exists, 
    partycongruent,
    partycongruent_neat,
    issue,
    position,
    send_cat,
    mp_type_detailed,
    mp_type_abstain_nonreb,
    diff_mp_party_outliers_removed,
    diff_mp_party,
    has_response_info,
    respondent_reported_letter_status,
    lr_mp_party,
    word_total) |>
  arrange(const_anon_id)

write_csv(main_small, 
          file = file.path(data_dir, "main.csv"))

bes17.dat_small <- bes17.dat|>
  dplyr::select(Winner17,
                leaveHanretty,
                Majority17,
                c11Degree,
                mp_first_start,
                frontbench,
                in_experiment,
                in_experimentn,
                Region)

write_csv(bes17.dat_small, 
          file = file.path(data_dir, "bes17.dat_small.csv"))
