

##----do-autoresponses-predict----
# reviewer comments that autoresponses may be a sign that MPs have detected experiment
# do autoresponses predict manual responses

amresponse_summary <- main |>
  group_by(constituency_name) |>
  summarise(n = n(),
            autoresponse_prop = sum(autoresponse_exists, na.rm =TRUE)/n,
            manualresponse_exists_inc_post_count_nonresponse = sum(manualresponse_exists_inc_post_count_nonresponse, na.rm=TRUE)/n) 

f1 <- lm(manualresponse_exists_inc_post_count_nonresponse ~ autoresponse_exists, main)
f1a <- lm(manualresponse_exists_inc_post ~ autoresponse_exists, main)
f2 <- lm(manualresponse_exists_inc_post_count_nonresponse ~ autoresponse_prop, amresponse_summary)
f3 <- lme4::lmer(manualresponse_exists_inc_post_count_nonresponse ~ autoresponse_exists + (1|constituency_name), main)
f4 <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ autoresponse_exists +factor(onsconstid), data=main, cluster=id)



huxtable::huxreg(f1, f1a, f2, f3, f4, coefs = c("(Intercept)",
                                           "autoresponse_exists",
                                           "autoresponse_prop"))

##----Further-Analysis-and-Tables-Not-in-Paper-----

##----response-rate-descriptives-party-----
main %>% 
  mutate(party=case_when(
    party == "lab" ~ "Labour",
    party == "con" ~ "Conservative",
    TRUE ~ "Other"
  )) %>%
  group_by(party) %>%
  summarise(
    substantive_response = scales::percent(mean(manualresponse_exists_inc_post_count_nonresponse), accuracy = .1)
  ) %>%
  rename(
    " " = party,
    "Response rate"=substantive_response
  ) %>%
  huxtable()%>% 
  set_bold(row = 1, col = everywhere) %>% 
  set_bottom_border(row = 1, col = everywhere,  brdr(.4, "solid", "black")) %>% 
  set_right_border(everywhere, 1,  brdr(.4, "solid", "black")) %>%
  # set_background_color(evens, everywhere, "grey95") %>%
  set_caption("Substantive response rate by MP party.")


