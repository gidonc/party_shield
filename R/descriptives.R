## This code produces information in the appendix and other descriptives. We should edit this to produce another file which reproduces only the material from the appendix

library(tidyverse)
library(huxtable)


const_with_multi_mps <-mp_rebel_dat %>% 
  group_by(ONSConstID, constituency_name, mp_name)%>%
  summarize() %>%
  group_by(ONSConstID, constituency_name) %>% 
  tally() %>%
  filter(n>1)

n_byelection_affected<-sum(new.anon$constituency %in% const_with_multi_mps$ONSConstID)




# This is the new version 

overall <- bes17.dat %>% 
  summarise(
    "Lab MP" = scales::percent(mean(Winner17==2), accuracy = .1),
    "Con MP" = scales::percent(mean(Winner17==1), accuracy = .1),
    "Oth MP" = scales::percent(mean(!Winner17 %in% c(1,2)), accuracy = .1),
    "Av.leave" = scales::percent(mean(leaveHanretty)/100, accuracy = .1),
    "Av.majority" = scales::percent(mean(Majority17)/100, accuracy = .1),
    "Av.degree" = scales::percent(mean(c11Degree, na.rm=TRUE)/100, accuracy =.1)
  ) %>%
  mutate(in_experiment = "GB MPs")

treats <- bes17.dat %>% 
  group_by(in_experiment) %>%
  summarise(
    "Lab MP" = scales::percent(mean(Winner17==2), accuracy = .1),
    "Con MP" = scales::percent(mean(Winner17==1), accuracy = .1),
    "Oth MP" = scales::percent(mean(!Winner17 %in% c(1,2)), accuracy = .1),
    "Av.leave" = scales::percent(mean(leaveHanretty)/100, accuracy = .1),
    "Av.majority" = scales::percent(mean(Majority17)/100, accuracy = .1),
    "Av.degree" = scales::percent(mean(c11Degree, na.rm=TRUE)/100, accuracy =.1)
  ) %>%
  filter(in_experiment=="Included (letters sent)") 


## ----balance-table-----
# Gives summary statistics on the political background of the British MPs included in our sample (first row) and all British MPs (second row)

bind_rows(treats, overall)%>%
  rename(" " = in_experiment) %>%
  huxtable()%>% 
  # set_background_color(evens, everywhere, "grey95") %>%
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  set_caption("Characteristics of constituencies contained in sample compared to all GB constituencies.") %>%
  set_label("tab:balance1")


## ----regional-distribution-setup----

ggbmps <- bes17.dat %>%
  as_factor() %>%
  mutate(in_experimentn="GB MPs") %>%
  mutate(n_condition=n()) %>%
  group_by(Region, n_condition, in_experimentn) %>%
  tally() %>%
  mutate(prop =scales::percent(n/n_condition)) %>%
  select(-n_condition, -n) %>%
  pivot_wider(names_from=in_experimentn, values_from=prop, values_fill = "0%") 

gtreats <- bes17.dat %>%
  as_factor() %>%
  group_by(in_experimentn) %>%
  mutate(n_condition=n()) %>%
  group_by(in_experimentn, Region, n_condition) %>%
  tally() %>%
  mutate(prop =scales::percent(n/n_condition)) %>%
  select(-n_condition, -n) %>%
  pivot_wider(names_from=in_experimentn, values_from=prop, values_fill = "0%") 


## ----regional-distribution-table----
# Geographic distribution of constituencies contin in sample compared to the distribution of all GB constituencies

gtreats %>%
  ungroup() %>%
  select(c("Region", "Included")) %>%
  left_join(ggbmps %>%
              ungroup() %>%
              select(c("Region", "GB MPs")), 
            by = "Region") %>%
  rename(" " = Region) %>%
  huxtable()%>% 
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("Geographic distribution of constituencies contained in sample compared to distribution of all GB constituencies.") %>%
  set_label("tab:geobalance")


## ----response-rate-descriptives----

main %>% 
  summarise(
    holding_rate = mean(response_auto_and_manual),
    holding_n = sum(response_auto_and_manual),
    substantive_rate = mean(manualresponse_exists_inc_post_count_nonresponse),
    substantive_n = sum(manualresponse_exists_inc_post_count_nonresponse)
  ) %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("variable", "name")) %>%
  pivot_wider(id_cols = variable  ) %>%
  mutate(rate = scales::percent(rate, accuracy = .1),
         variable = dplyr::recode(variable,
                                  "holding"="Holding and substantive responses",
                                  "substantive"= "Substantive responses only")) %>%
  rename(
    " " =variable,
    "Response rate" = rate
  ) %>%
  
  huxtable()%>% 
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("Overall MP response rates including holding responses and restricted to substantive responses only.") %>%
  set_label("tab:rrautoandman")


##----response-rate-sendorder---

main %>% 
  mutate(send_position_red = case_when(
    send_position == 1 ~ "1",
    send_position %in% c(2:6) ~ "2-6",
    TRUE ~ "beyond 6"
  )) %>%
  group_by(send_position_red) %>%
  summarise(
    substantive_response = scales::percent(mean(manualresponse_exists_inc_post_count_nonresponse), accuracy = .1)
  ) %>%
  rename(" " = send_position_red,
         "Response rate" = substantive_response) %>%
  huxtable()%>% 
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("Substantive response rate by order in which we sent letters to MPs.")

## ----response-rate-by-issue----

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
main %>% 
  left_join(issue_nice) %>%
  group_by(issue_nice, issue_order) %>%
  summarise(
    "Letters sent" = n(),
    "Response rate" = scales::percent(mean(manualresponse_exists_inc_post_count_nonresponse), accuracy =.1)
  ) %>%
  arrange(issue_order) %>%
  rename(
    " " = issue_nice
  ) %>%
  ungroup() %>%
  select(-issue_order) %>%
  huxtable()%>% 
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("MP substantive response rate to constituent letters by policy area.")

## ----response-rate-by-issue-plot----

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
main %>% 
  left_join(issue_nice) %>%
  group_by(issue_nice, issue_order) %>%
  summarise(
    "letters_sent" = n(),
    "response_rate" = 100*mean(manualresponse_exists_inc_post_count_nonresponse)
  ) %>%
  mutate(
    prop = response_rate/100,
    se = sqrt((prop*(1-prop))/letters_sent),
    cilo = 100*(prop - 1.96*se),
    cihi = 100*(prop + 1.96*se)
  ) %>%
  ungroup() %>%
  mutate(
    issue_nice = factor(paste0(issue_nice, "\n(n = ", letters_sent, ")")),
    issue_nice = reorder(issue_nice, response_rate)
  ) %>%
  ggplot() + 
  geom_bar(aes(x = issue_nice, y = response_rate), stat = "identity", fill = "gray60") + 
  geom_errorbar(aes(x = issue_nice, ymin = cilo, ymax = cihi), width = 0.4) + 
  coord_flip() + 
  xlab("") + 
  ylab("Response Rate (%)") + 
  ylim(0,100) + 
  theme_bw()



##----response-rate-by-brexit-issue----

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
main %>% 
  left_join(issue_nice) %>%
  mutate(is_brexit = issue_order < 6) %>%
  group_by(is_brexit) %>%
  summarise(
    "Letters sent" = n(),
    "Response rate" = scales::percent(mean(manualresponse_exists_inc_post_count_nonresponse), accuracy =.1)
  ) %>%
  ungroup() %>%
  huxtable()%>% 
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("MP substantive response rate to constituent letters by Brexit and non-Brexit.")
