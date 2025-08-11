

## ----setup-party-shield----
library(tidyverse)
library(randomizr)
library(estimatr)
library(ri2)
library(readstata13)
library(broom)
library(huxtable)
library(patchwork)
library(readr)


options(scipen=999)


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


## ----load-party-shield-processed-data-local----

parent_dir <- here::here()
data_dir <- file.path(parent_dir, "Data")

main <- read_csv(file.path(data_dir, "main.csv"))  
bes17.dat <- read_csv(file.path(data_dir, "bes17.dat_small.csv"))  

##----load-party-shield-processed-data-Rmd----
## This code will be run by the Rmd file - run code above to load data manually
main <- read_csv("../Data/main.csv")
bes17.dat <- read_csv("../Data/bes17.dat_small.csv")


## ----response-rate-descriptives----
# Overall MP response rates including holding responses and restricted to substantive responses only

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


## ----response-rate-on-congruence-----

## Similar Table to Grose et al.
##  Table 3 in paper
# Do MPs Respond Differentially to Letters their Party Agrees/Disagrees with?

prop.test1 <- main %>% 
  mutate(sub_resp = ifelse(manualresponse_exists_inc_post_count_nonresponse==1, "yes", "no")) %>%
  group_by(partycongruent, sub_resp) %>%
  tally() %>%
  pivot_wider(names_from=sub_resp, values_from=n) %>%
  ungroup() %>%
  dplyr::select(no, yes) %>%
  as.matrix() %>%
  prop.test()
diff1 <- mean(main$manualresponse_exists_inc_post_count_nonresponse[main$partycongruent==1])-mean(main$manualresponse_exists_inc_post_count_nonresponse[main$partycongruent==0])

p.val1 <- prop.test1$p.value

main %>% 
  mutate(partycongruent = ifelse(partycongruent==1, "Congruent: Letter support party position", "Incongruent: Letter opposes party position"))%>%
  group_by(partycongruent) %>%
  summarise(
    substantive_response = scales::percent(mean(manualresponse_exists_inc_post_count_nonresponse), accuracy = .1)
  ) %>%
  rename(
    " " = partycongruent,
    "Response rate"=substantive_response
  ) %>%
  huxtable() %>%
  insert_row(
    c("difference", format(round(abs(diff1*100), 2), nsmall = 2)),
    after=3
  ) %>%
  insert_row(
    c("p.value", format(round(p.val1, 2), nsmall=2)),
    after=4
  )%>%
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("Do MPs Respond Differentially to Letters their Party Agrees/Disagree with?")


##----lin-mod-responsiveness----
# run and report linear probability models of MP responsiveness

fit1.1<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)

fit1.2<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id)+factor(send_cat), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)


reg_table_1<-huxtable::huxreg('Full sample' = fit1.1, 
                     'Loyalist MPs' = fit1.2,
                     coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("Linear probability models of MP responsiveness.")

label(reg_table_1) <- "tab:response-regs-main"

reg_table_1


##----lin-responsiveness-varyresponsetypes----
# run and report regression using different definitions of responsiveness

fit2.1<-lm_robust(has_response_info~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)
fit2.2<-lm_robust(respondent_reported_letter_status~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)
fit2.3<-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)

reg_table_2<-huxtable::huxreg('Letter Status Reported' = fit2.1, 
                     'Letter Response' = fit2.2, 
                     'Email Response' = fit2.3,
                     coefs = c("Party congruent" = "partycongruent")) %>%
  insert_row("MP fixed effects", "yes", "yes", "yes", after=3, copy_cell_props = FALSE) %>%
  insert_row("position fixed effects", "yes", "yes", "yes", after=4, copy_cell_props = FALSE) %>%
  insert_row("send order fixed effects", "yes",  "yes", "yes", after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:4, 1) %>%
  set_bottom_border(4, 2:4, 0) %>%
  set_bottom_border(6, 2:4, 1) %>%
  set_align('centre')  %>%
  set_caption("Linear probability models of: whether constituent reports letter response status; MP letter response among cases where status is known; MP email response among cases where it is known.")

label(reg_table_2) <- "tab:response-regs-letters"


reg_table_2

##----OLS-self-v-party----

fit3.1 <- lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)

fit3.2 <- lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id)+factor(send_cat), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)


reg_table_3 <-huxtable::huxreg('Full sample' = fit3.1, 
                     'Loyalist MPS' = fit3.2, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("OLS models of log ratio of MP vs party mentions in observed responses")

label(reg_table_3) <- "tab:text-regs-main"


reg_table_3

##-----appendix-material----


## ----balance-table-----
# Gives summary statistics on the political background of the British MPs included in our sample (first row) and all British MPs (second row)

overall <- bes17.dat %>% 
  summarise(
    "Lab MP" = scales::percent(mean(Winner17==2), accuracy = .1),
    "Con MP" = scales::percent(mean(Winner17==1), accuracy = .1),
    "Oth MP" = scales::percent(mean(!Winner17 %in% c(1,2)), accuracy = .1),
    "Frontbench" = scales::percent(mean(frontbench), accuracy = .1),
    "Av.leave" = scales::percent(mean(leaveHanretty)/100, accuracy = .1),
    "Av.majority" = scales::percent(mean(Majority17)/100, accuracy = .1),
    "Av.degree" = scales::percent(mean(c11Degree, na.rm=TRUE)/100, accuracy =.1),
    "Service (years)" =as.character(round(mean(service_duration*10), 1))
  ) %>%
  mutate(in_experiment = "GB MPs")|>
  pivot_longer(cols = -in_experiment, values_to = "GB MPs")  |>
  dplyr::select(-in_experiment)

treats <- bes17.dat %>% 
  group_by(in_experiment) %>%
  summarise(
    "Lab MP" = scales::percent(mean(Winner17==2), accuracy = .1),
    "Con MP" = scales::percent(mean(Winner17==1), accuracy = .1),
    "Oth MP" = scales::percent(mean(!Winner17 %in% c(1,2)), accuracy = .1),
    "Frontbench" = scales::percent(mean(frontbench), accuracy = .1),
    "Av.leave" = scales::percent(mean(leaveHanretty)/100, accuracy = .1),
    "Av.majority" = scales::percent(mean(Majority17)/100, accuracy = .1),
    "Av.degree" = scales::percent(mean(c11Degree, na.rm=TRUE)/100, accuracy =.1),
    "Service (years)" =as.character(round(mean(service_duration*10), 1))
  ) %>%
  filter(in_experiment=="Included (letters sent)") |>
  pivot_longer(cols = -in_experiment, values_to = "Included (letters sent)") |>
  dplyr::select(-in_experiment)

left_join(overall, treats) |>
  dplyr::rename(" "=name) |>
  huxtable()  |>
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  set_caption("Characteristics of constituencies contained in sample compared to all GB constituencies.") %>%
  set_label("tab:balance1")


## ----regional-distribution-setup----

ggbmps <- bes17.dat %>%
  as_tibble() %>%
  haven::as_factor() %>%
  mutate(in_experimentn="GB MPs") %>%
  mutate(n_condition=n()) %>%
  group_by(Region, n_condition, in_experimentn) %>%
  tally() %>%
  mutate(prop =scales::percent(n/n_condition)) %>%
  dplyr::select(-n_condition, -n) %>%
  pivot_wider(names_from=in_experimentn, values_from=prop, values_fill = "0%") 

gtreats <- bes17.dat %>%
  as_tibble() %>%
  haven::as_factor() %>%
  group_by(in_experimentn) %>%
  mutate(n_condition=n()) %>%
  group_by(in_experimentn, Region, n_condition) %>%
  tally() %>%
  mutate(prop =scales::percent(n/n_condition)) %>%
  dplyr::select(-n_condition, -n) %>%
  pivot_wider(names_from=in_experimentn, values_from=prop, values_fill = "0%") 


## ----regional-distribution-table----
# Geographic distribution of constituencies contin in sample compared to the distribution of all GB constituencies

gtreats %>%
  ungroup() %>%
  dplyr::select(c("Region", "Included")) %>%
  left_join(ggbmps %>%
              ungroup() %>%
              dplyr::select(c("Region", "GB MPs")), 
            by = "Region") %>%
  mutate(Region = as_factor(Region)) |>
  rename(" " = Region) %>%
  huxtable()%>% 
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("Geographic distribution of constituencies contained in sample compared to distribution of all GB constituencies.") %>%
  set_label("tab:geobalance")




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


##----lin-responsiveness-alternativeloyalist----
# MP responsiveness regressions using alternative coding of MP loyalty

fitSI.1.1_abstain_nonreb<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)

fitSI.1.2_abstain_nonreb<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id)+factor(send_cat), data=filter(main, mp_type_abstain_nonreb=="loyalist"), cluster=id)

reg_table_SI5<-huxtable::huxreg('Full sample' = fitSI.1.1_abstain_nonreb, 
                                'Loyalist MPs' = fitSI.1.2_abstain_nonreb,
                                coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("Linear probability models of MP responsiveness.")

label(reg_table_SI5) <- "tab:response-regs-abstain-nonreb"

reg_table_SI5


##----OLS-self-v-party-alternativeloyalist----


fitSI.2.1_abstain_nonreb<-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)


fitSI.2.2_abstain_nonreb<-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id)+factor(send_cat), data=filter(main, mp_type_abstain_nonreb=="loyalist"), cluster=id)


reg_table_SI6 <- huxtable::huxreg('Full sample' = fitSI.2.1_abstain_nonreb, 
                     'Loyalist MPS' = fitSI.2.2_abstain_nonreb, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("OLS models of log ratio of MP vs party mentions in observed responses")

label(reg_table_SI6) <- "tab:text-regs-abstain-nonreb"


reg_table_SI6

## ----response-rate-by-issue-plot----

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

##----response-rate-sendorder----

main %>% 
  group_by(send_cat) %>%
  summarise(
    substantive_response = scales::percent(mean(manualresponse_exists_inc_post_count_nonresponse), accuracy = .1)
  ) %>%
  rename(" " = send_cat,
         "Response rate" = substantive_response) %>%
  huxtable()%>% 
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("Substantive response rate by order in which we sent letters to MPs.")


##----lin-responsiveness-brexit----

fitSI.8.1 <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), 
                      data=main %>%
                        filter(issue %in% c("customs", "singlem",
                                            "freedomm", "regulation", 
                                            "secondr")), 
                      cluster=id)


fitSI.8.2 <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), 
                         data=main %>%
                           filter(!(issue %in% c("customs", "singlem",
                                                 "freedomm", "regulation", 
                                                 "secondr"))), 
                         cluster=id)

reg_table_SI8<-huxtable::huxreg('Brexit letters' = fitSI.8.1,
                     'Non-Brexit letters' = fitSI.8.2, 
                     coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("Linear probability models of MP responsiveness for Brexit and non-Brexit letters, separately.")

label(reg_table_SI8) <- "tab:response-regs-by-brexit"


reg_table_SI8

##----dist-lr-by-congruence----

SIFig3 <- ggplot(main, aes(x = lr_mp_party)) + 
  geom_density(aes(fill = partycongruent_neat, 
                   linetype = partycongruent_neat), 
               alpha = 0.2) +
  labs(x = "Log-ratio[MP vs party mentions]", y = "") + 
  scale_fill_discrete(type = c("gray80", "gray50")) + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(), 
        legend.position = "right")

SIFig3

##----lin-mpparty-standardised----
# OLS models of difference in number of MP mentions and party mentions standardised by number of words in the response

fitSI.9.1<-lm_robust(diff_mp_party_outliers_removed~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)

fitSI.9.2<-lm_robust(diff_mp_party_outliers_removed~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id)+factor(send_cat), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)




reg_table_SI9<-huxtable::huxreg('Full Sample' = fitSI.9.1, 
                     'Loyalist MPs' = fitSI.9.2, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("OLS models of  difference in number of MP mentions and party mentions, both standardised by overall number of words in the letter.")

label(reg_table_SI9) <- "tab:text-regs-diff"


reg_table_SI9

##----reg-letter-length----

fitSI.10.1 <- lm_robust(I(log(word_total)) ~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id) + factor(send_cat), data=main, cluster=id)

fitSI.10.2 <- lm_robust(I(log(word_total))~ partycongruent + factor(issue)*factor(position) + factor(const_anon_id)+factor(send_cat), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)


reg_table_SI10<-huxtable::huxreg('Full Sample' = fitSI.10.1, 
                     'Loyalist MPs' = fitSI.10.2, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("OLS models of log word length of MP response text.")

label(reg_table_SI10) <- "tab:text-regs-length"


reg_table_SI10



