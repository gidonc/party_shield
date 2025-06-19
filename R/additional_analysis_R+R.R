library(tidyverse)
library(huxtable)

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


##----descriptive-responsiveness-analysis----

names(main)

main2 <- main |>
  left_join(as_factor(bes17.dat), by = c("constituency"="ONSConstID")) |>
  mutate(win17 = as_factor(Winner17),
         win15 = as_factor(Winner15),
         cont = as.character(win15)==as.character(win17),
         MPsex = case_when(
           win17 == "Conservative" ~ ConPPCsex17,
           win17 == "Labour" ~ LabPPCsex17,
           win17 == "Liberal Democrat" ~ LDPPCsex17,
           win17 == "Green" ~ GreenPPCsex17,
         ),
         retired = c11Age65to74+c11Age75to84+c11Age85to89+c11Age90plus,
         children = c11Age0to4 + c11Age5to7+c11Age8to9+c11Age10to14+c11Age15+c11Age16to17
  ) |>
  mutate(has_resp = manualresponse_exists_inc_post_count_nonresponse) |>
  group_by(constituency) |>
  mutate(
    n_letters = n()
  )
rr_const <- main |>
  group_by(constituency) |>
  summarise(
    response_rate = mean(manualresponse_exists_inc_post_count_nonresponse),
    n_letters = n(),
    n_responses = sum(manualresponse_exists_inc_post_count_nonresponse),
    rebel_prop = mean(mp_type == "rebel")
  ) |>
  left_join(as_factor(bes17.dat), by = c("constituency"="ONSConstID")) |>
  mutate(win17 = as_factor(Winner17),
         win15 = as_factor(Winner15),
         cont = as.character(win15)==as.character(win17),
         MPsex = case_when(
           
           win17 == "Conservative" ~ ConPPCsex17,
           win17 == "Labour" ~ LabPPCsex17,
           win17 == "Liberal Democrat" ~ LDPPCsex17,
           win17 == "Green" ~ GreenPPCsex17,
         ),
         retired = c11Age65to74+c11Age75to84+c11Age85to89+c11Age90plus,
         children = c11Age0to4 + c11Age5to7+c11Age8to9+c11Age10to14+c11Age15+c11Age16to17
         )

# could do with data for:
# MP Localness based on place of birth (log(distance) of place of birth from the constituency) - Bolet and Campbell find that localness is not generally related to responsiveness, but there is a significant interaction of localness and partisanship (at the .1 level)
# Rural/Urban
# Further MP characteristics: MP seniority

hist(rr_const$response_rate)
f1 <- lm(response_rate ~ c11CarsTwo, rr_const)
f2 <- lm(response_rate ~ c11Degree, rr_const)



huxreg(f1, f2)
library(arm)
mf1 <- lmer(has_resp ~ win17 + MPsex + mp_type+ c11Degree + send_position_red + I(Majority17<10) + (1|constituency), main2)
mf2 <- lmer(has_resp ~ win17 + retired + c11Degree + c11PopulationDensity + (1|constituency), main2)

huxreg(mf1, mf2)

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




Here are the model estimates when we break the sample down into all four types of case (loyalist, moderate rebel, extreme rebel and all cases):
  
  
  aa<-huxtable::huxreg('Loyalists' = H1, 'Mod rebels' = H3,
                       'Extreme rebels' = H5,  
                       'All' = H7, 
                       coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 4), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:5, 1) %>%
  set_bottom_border(4, 2:5, 0) %>%
  set_bottom_border(6, 2:5, 1) %>%
  set_align('centre')

aa


# To get a sense of how precise these estimates are, we provide coefficient plots which show 95\% confidence intervals. For ease of quoting numbers in the write-up, we also print a table which shows the numeric values of the CI bounds. 

coef_tab <- bind_rows(
  tidy(H1, conf.int = TRUE) %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Loyalists"),
  tidy(H3, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Moderate rebels"),
  tidy(H5, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Extreme rebels"),
  tidy(H7, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "All")
) %>%
  mutate(model = factor(model, levels = rev(c("Loyalists", "Moderate rebels", "Extreme rebels", "All"))))

coef_tab %>% select(!(c(outcome, p.value, df)))


coef_tab %>%
  filter(model %in% c("Loyalists", "All")) %>%
  ggplot(aes(x = model, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point(shape = 21, fill = "white", size = 1.5) + 
  coord_flip() + 
  labs(y = "Estimated effect of\nconstituent-party congruence",
       x = "") +
  theme_bw()

```


coef_tab %>%
  ggplot(aes(x = model, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point(shape = 21, fill = "white", size = 1.5) + 
  coord_flip() + 
  labs(y = "Estimated effect of\nconstituent-party congruence",
       x = "") +
  theme_bw()

```


\newpage 

## Robustness checks

Now we re-run the main analysis of MP response separately for Brexit-related letters and non-Brexit-related letters. Because of the small sample sizes when we break down to these types of letter, we only run one models pooling all types of case (loyalist and rebel) for each type of letter.

```{r response-models-brexit-split, warning=FALSE, message=FALSE, echo = FALSE}


```

Confidence intervals

```{r response-by-brexit-coefplot-prep, warning=FALSE, message=FALSE, echo = FALSE}

coef_tab <- bind_rows(
  tidy(H7_brexit, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "All, Brexit letters"),
  tidy(H7_nonbrexit, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "All, non-Brexit letters")
) %>%
  mutate(model = factor(model, levels = rev(c("All, Brexit letters", "All, non-Brexit letters"))))

coef_tab %>% select(!(c(outcome, p.value, df)))


```


Look at interaction of Brexit/non-Brexit topic with treatment effect.

```{rresponse-models-brexit-interaction, warning=FALSE, message=FALSE, echo = FALSE}

main <- main %>%
  mutate(brexit_issue = as.numeric(issue %in% c("customs", "singlem",
                                                "freedomm", "regulation", 
                                                "secondr")))

H7_brexit_interaction <- lm_robust(manualresponse_exists_inc_post_count_nonresponse ~  partycongruent*brexit_issue + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data = main,
                                   cluster=id)

aa<-huxtable::huxreg('Brexit letters interaction' = H7_brexit_interaction,
                     coefs = c("Party congruent"= "partycongruent",
                               "Brexit issue" = "brexit_issue",
                               "Congruent X Brexit" = "partycongruent:brexit_issue")) %>%
  insert_row("MP FE?", rep("yes", 1), after=7, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 1), after=8, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 1), after=8, copy_cell_props = FALSE) %>%
  set_align('centre') %>%
  set_caption("Linear probability models of MP responsiveness for Brexit and non-Brexit letters.")

label(aa) <- "tab:response-regs-by-brexit"


aa

```


\newpage

The next two tables are robustness checks which use email response/email non-response as our indicator of receipt (we have complete information on this)

```{r response-models-email-detailed, warning=FALSE, message=FALSE, echo = FALSE}

H1<-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)
H3 <-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="moderate rebel"), cluster=id)
H5 <-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="extreme rebel"), cluster=id)
H7<-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg('Loyalists' = H1, 'Mod rebels' = H3,
                     'Extreme rebels' = H5,  
                     'All' = H7, 
                     coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 4), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:5, 1) %>%
  set_bottom_border(4, 2:5, 0) %>%
  set_bottom_border(6, 2:5, 1) %>%
  set_align('centre') 

aa


```

```{r response-models-email-simple, warning=FALSE, message=FALSE, echo = FALSE}

H1<-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)
H3 <-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="moderate rebel"), cluster=id)
H5 <-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="extreme rebel"), cluster=id)
H7<-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg(
  'Full sample' = H7, 
  'Loyalist MPs' = H1, 
  coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre')  %>%
  set_caption("Linear probability models of MP email responsiveness.")

label(aa) <- "tab:response-regs-email"


aa


```

\newpage

## Checking Assumptions

We were concerned about whether the likelihood of a respondent telling us about their letter status associated with the letter being party-congruent. Models 1 and 2 below provide a potential test for this.

We subset to non missing respondent reported letter status and run a regression on this status.

In both cases the first model is simple OLS (no covariates, no fixed effects) and second model is the model used in main analysis.

# Analysis of MP response text

## A note on missingness

This shows there are no cases where we have text analysis but don't record a response. However, there are 37 cases where we know there was a response but we have no text analysis for the response (presumably there are 37 cases where we didn't get a photo of the letter?).


```{r, echo = TRUE}

with(main,table(manualresponse_exists_inc_post_count_nonresponse, !is.na(sum_i)))

with(main,table(manualresponse_exists_inc_post_count_nonresponse, !is.na(sum_i), respondent_reported_letter_status))


```



## Descriptives

We begin by assessing the distribution of our key text measures in our data. 

```{r text-dist-plots-simple, fig.caption = "Distribution of log-ratio of MP versus party mentions in observed MP response texts, conditional on constituent-party policy congruence.", fig.width=8, fig.height=4, echo=FALSE, warning=FALSE, message=FALSE, echo=FALSE}



```



```{r text-dist-plots, fig.caption = "Distribution of key features of MP response texts", fig.width=8, fig.height=6, echo=FALSE, warning=FALSE, message=FALSE, echo=FALSE}



main <- main %>% 
  mutate(partycongruent_neat = recode(partycongruent, 
                                      `0` = "Constituent-party not congruent",
                                      `1` = "Constituent-party congruent"))

p1 <- ggplot(main, aes(x = word_total)) + 
  geom_density(aes(fill = partycongruent_neat, 
                   linetype = partycongruent_neat), 
               alpha = 0.2) +
  labs(x = "Total words", y = "") + 
  scale_fill_discrete(type = c("gray80", "gray50")) + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank())

p2 <- ggplot(main, aes(x = sum_i)) + 
  geom_density(aes(fill = partycongruent_neat, 
                   linetype = partycongruent_neat), 
               alpha = 0.2) +
  labs(x = "Total self mentions", y = "") + 
  scale_fill_discrete(type = c("gray80", "gray50")) + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank())

p3 <- ggplot(main, aes(x = sum_party_mentions)) + 
  geom_density(aes(fill = partycongruent_neat, 
                   linetype = partycongruent_neat), 
               alpha = 0.2) +
  labs(x = "Total party mentions", y = "") + 
  scale_fill_discrete(type = c("gray80", "gray50")) + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank())

p4 <- ggplot(main, aes(x = lr_mp_party)) + 
  geom_density(aes(fill = partycongruent_neat, 
                   linetype = partycongruent_neat), 
               alpha = 0.2) +
  labs(x = "Log-ratio[self vs party]", y = "") + 
  scale_fill_discrete(type = c("gray80", "gray50")) + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank())

p1 + p2 + p3 + p4 + guide_area() + plot_layout(ncol = 2, guides = "collect")

```




To get a sense of how precise these estimates are, we provide coefficient plots which show 95\% confidence intervals. For ease of quoting numbers in the write-up, we also print a table which shows the numeric values of the CI bounds. 


```{r text-coefplot-prep, warning=FALSE, message=FALSE, echo=FALSE}

coef_tab <- bind_rows(
  tidy(H2, conf.int = TRUE) %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Loyalists"),
  tidy(H4, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Moderate rebels"),
  tidy(H6, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Extreme rebels"),
  tidy(H8, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "All")
) %>%
  mutate(model = factor(model, levels = rev(c("Loyalists", "Moderate rebels", "Extreme rebels", "All"))))

coef_tab %>% select(!(c(outcome, p.value, df)))


```

```{r text-coefplot-simple, fig.cap="Coefficient plot for MP vs party emphasis models", fig.width=6, fig.height=3, echo=FALSE}

coef_tab %>%
  filter(model %in% c("Loyalists", "All")) %>%
  ggplot(aes(x = model, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point(shape = 21, fill = "white", size = 1.5) + 
  coord_flip() + 
  labs(y = "Estimated effect of\nconstituent-party congruence",
       x = "") +
  theme_bw()

```


```{r text-coefplot-detailed, fig.cap="Coefficient plot for MP vs party emphasis models", fig.width=6, fig.height=4, echo=FALSE}

coef_tab %>%
  ggplot(aes(x = model, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point(shape = 21, fill = "white", size = 1.5) + 
  coord_flip() + 
  labs(y = "Estimated effect of\nconstituent-party congruence",
       x = "") +
  theme_bw()

```


\newpage 

As an alternative, to ease interpretation further we plot the exponentiated coefficients. These tell us the estimated multiplicative change in the ratio of MP to party mentions when the constituent switches from non-congruent to congruent with the party. 


```{r text-coefplot-prep-exp, warning=FALSE, message=FALSE, echo=FALSE}

coef_tab <- bind_rows(
  tidy(H2, conf.int = TRUE) %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Loyalists"),
  tidy(H4, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Moderate rebels"),
  tidy(H6, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "Extreme rebels"),
  tidy(H8, conf.int = TRUE)  %>%
    filter(term == "partycongruent") %>%
    mutate(model = "All")
) %>%
  mutate(model = factor(model, levels = rev(c("Loyalists", "Moderate rebels", "Extreme rebels", "All"))),
         exp.estimate = exp(estimate),
         exp.conf.low = exp(conf.low),
         exp.conf.high = exp(conf.high))

coef_tab %>% select((c(term, exp.estimate, exp.conf.low, exp.conf.high)))


```

```{r text-coefplot-simple-exp, fig.cap="Coefficient plot for MP vs party emphasis models", fig.width=6, fig.height=3, echo=FALSE}

coef_tab %>%
  filter(model %in% c("Loyalists", "All")) %>%
  mutate(model = recode(model, "Loyalists" = "Loyalist MPs", "All" = "Full Sample"),
         model = factor(model, levels = rev(c("Full Sample", "Loyalist MPs")))) %>%
  ggplot(aes(x = model, y = exp.estimate)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_linerange(aes(ymin = exp.conf.low, ymax = exp.conf.high)) + 
  geom_point(shape = 21, fill = "white", size = 1.5) + 
  coord_flip() + 
  labs(y = "Estimated multiplicative effect of\nconstituent-party congruence\non ratio of MP vs party mentions",
       x = "") +
  theme_bw()

```


```{r text-coefplot-detailed-exp, fig.cap="Coefficient plot for MP vs party emphasis models", fig.width=6, fig.height=4, echo=FALSE}

coef_tab %>%
  ggplot(aes(x = model, y = exp.estimate)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_linerange(aes(ymin = exp.conf.low, ymax = exp.conf.high)) + 
  geom_point(shape = 21, fill = "white", size = 1.5) + 
  coord_flip() + 
  labs(y = "Estimated multiplicative effect of\nconstituent-party congruence\non ratio of MP vs party mentions",
       x = "") +
  theme_bw()

```



\newpage

### Extra models of text measures for appendix

We re-run the main text measure model using an alternative dependent variable: the difference in number of MP mentions of and party mentions, both standardised by overall number of words in the letter. For this measure we remove outliers (observations where the score lies beyond 1.5 x IQR from the upper or lower quartile).

```{r text-regs-alt1-detailed, echo=FALSE, warning = FALSE, message=FALSE}



```



\newpage

As a further check, we estimate models where the DV is the log word lengtth of the letter.


```{r text-regs-alt2-detailed, echo=FALSE, warning = FALSE, message=FALSE}


```



\newpage


As a final check, we re-run the main analysis on the log ratio of MP vs party mentions separately for Brexit-related letters and non-Brexit-related letters. Because of the small sample sizes when we break down to these types of letter, we only run one models pooling all types of case (loyalist and rebel) for each type of letter.

```{r text-models-brexit-split, warning=FALSE, message=FALSE, echo=FALSE}


H8_brexit <-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), 
                      data=main %>%
                        filter(issue %in% c("customs", "singlem",
                                            "freedomm", "regulation", 
                                            "secondr")), 
                      cluster=id)


H8_nonbrexit <-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), 
                         data=main %>%
                           filter(!(issue %in% c("customs", "singlem",
                                                 "freedomm", "regulation", 
                                                 "secondr"))), 
                         cluster=id)

aa<-huxtable::huxreg('Brexit' = H8_brexit,
                     'Non-Brexit' = H8_nonbrexit, 
                     coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("Linear probability models of log ratio of MP vs party mentions, estimated separately for Brexit-related letters and non-Brexit-related letters")

label(aa) <- "tab:text-regs-by-brexit"


aa


```



