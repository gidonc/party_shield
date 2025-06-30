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
                in_experiment,
                in_experimentn,
                Region)

write_csv(bes17.dat_small, 
          file = file.path(data_dir, "bes17.dat_small.csv"))
