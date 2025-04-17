
## ----setup-party-shield----


library(tidyverse)
library(randomizr)
library(estimatr)
library(ri2)
library(readstata13)
library(broom)
library(huxtable)
library(patchwork)


options(scipen=999)


### Response Rate Descriptives

# (2) Results: other papers overestimate the amount of response, no random assignment.[GC]
# Present response rate in tabular/graph form
# Explicit response result for autoresponse v manual response (overall, auto, substantive)
# 
# Response by send order (numbers of email received by the MPs from our project)
# Response by time
# Response by letter topic
# Response by party (Lab/Con/Oth to avoid id-ing Lucas or MPs from other small-N parties) [check that party is coded at the time letter was sent)
# 
# Of the 624 constituent emails sent, we received substantive email responses (i.e., not just holding replies) from 242 and were informed by constituents of postal responses (again substantive) in a further 44 cases. Thus, as shown in Table \ref{tab:rrautoandman}, our measured response rate is about 46%. This is much lower than the response rates of around 90% reported by Habel and Birch (2019) in their study of constituency service requests. They included holding replies as a response in their measures. Including holding responses does increase our response rate to about 63%, which is still much lower than they reported. Possibly the difference in response rates was influenced by the different policy environments, and the increase in volumes of correspondence which MPs reported receiving as a result of the debates about Brexit. 
# 
# The average responsiveness in our study is more consistent with – albeit slightly lower than – the 51% average reported response rate of UK MPs to constituent messages sent via the WriteToThem.org.uk for 2015-16, the last year for which such data is publicly available.[1] Response rates of this magnitude are lower than those reported in the Grose et al (2015) audit study of US Senators, suggesting that UK MPs may be less responsive than their US counterparts, perhaps because of the staffing discrepancies discussed above.


```{r}


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
```

\newpage 


## ----response-rate-on-congruence-----

## Similar Table to Grose et al.
##  Table 3 in paper

# (3) Results: “Can we provide a similar table as Grose, Malhotra, and Parks Van Houweling (2015) p.731 Tab 1 before jumping into estimations?” [Their table maps % of letters giving MC position by treatment group] [GC]
# Take this to mean response rate by party congruent
# Show % responding for party-congruence categories
# Show diff in % and P-value

prop.test1 <- main %>% 
  mutate(sub_resp = ifelse(manualresponse_exists_inc_post_count_nonresponse==1, "yes", "no")) %>%
  group_by(partycongruent, sub_resp) %>%
  tally() %>%
  pivot_wider(names_from=sub_resp, values_from=n) %>%
  ungroup() %>%
  select(no, yes) %>%
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



# This code replaces send order with reduced category send order, so that regressions are better powered
main <- main %>% 
  mutate(send_position = case_when(
    send_position == 1 ~ "1",
    send_position %in% c(2:6) ~ "2-6",
    TRUE ~ "beyond 6"
  ))  

## Analysis of MP Responsiveness

To estimate when MPs responded to constituents' request we estimate models of the following form:
\begin{equation}
    \text{reply}_i = \beta \text{party congruent}_i + \alpha \text{MP}_i + \tau \text{send order}_i + \phi \text{position}_i + \epsilon_i
\end{equation}
\noindent Our unit of analysis is the constituent letter $i$. We regress our measure of whether an MP replies to the letter (reply$_i$) on a binary indicator for whether the letter takes a position congruent with the MP's party (party congruent$_i$) using OLS regression. $\alpha$, $\tau$, $\phi$ are fixed effects for MPs, the time order of the send letter to each MP, and the position of the letter respectively. We include three types of fixed effects in the specification. First, since some MPs may be generally more responsive than others, we include MP fixed effects. Second, to account for MPs being more or less likely to respond to constituent letters send later in our audit study, we include fixed effects for the position of the letter in the sequence of letters sent to the MP (send order fixed effects). Third, since MPs may on average be more or less likely to respond to letters taking certain positions on certain issues (e.g. letters opposing immigration), we include fixed effects for each position on each issue. We cluster standard errors on the treatment assignment -- letter-sender.\footnote{These model specifications differ from those in our pre-registered plan. First, that plan specified a model that included predictors measuring letter congruence with party position, with MP position and the interaction of the two, and which was to be estimated only on the full sample. We now include only the first of these predictors in the model because the measurement of individual MP positions on issues proved infeasible. Rather, we were able to confidently measure a more limited variable: whether an MP's position on an issue relative to their party, as indicated by legislative dissent. Because we expect different effects of party congruent$_i$ depending on the MP's dissent status relative to their party on an issue, we now subset the data by this measure for certain tests. Second, whereas our pre-registered plan specified models including separate fixed effects for each MP and each issue, we amended the latter to fixed effects for each positions on each issue and also added the send order fixed effects. These changes were made because we believe the amended fixed effects better account for systematic features of the data generating process, thereby dealing with potential confounding or improving efficiency.} 

## Model Results

Our main results count the combined letter/email receipt as a response, and code all cases where we have not received either a letter or an email as a non-response.


Now we present a simplified version of the table which shows results for loyalist cases (by far the largest subsample) and all cases:
  
  ```{r response-models-simple, warning=FALSE, message=FALSE, echo = FALSE}

H1<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)

H3 <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="moderate rebel"), cluster=id)

H5 <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="extreme rebel"), cluster=id)

H7<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg('Full sample' = H7, 
                     'Loyalist MPs' = H1,
                     coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("Linear probability models of MP responsiveness.")

label(aa) <- "tab:response-regs-main"

aa


```


Now check results are similar when we classify loaylist MPs based on a vote-coding scheme which never code abstentions as rebellions. 

```{r response-models-simple-abstain-nonreb, warning=FALSE, message=FALSE, echo = FALSE}

H1_abstain_nonreb<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_abstain_nonreb=="loyalist"), cluster=id)

H7_abstain_nonreb<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg('Full sample' = H7_abstain_nonreb, 
                     'Loyalist MPs' = H1_abstain_nonreb,
                     coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("Linear probability models of MP responsiveness.")

label(aa) <- "tab:response-regs-abstain-nonreb"

aa


```


<!-- Here are the models again but with the reduced send order fixed effects:  -->
  <!-- ```{r response-models-simple-sendred, warning=FALSE, message=FALSE, echo = FALSE, eval=FALSE} -->
  
  <!-- H1<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position_red), data=filter(main, mp_type_detailed=="loyalist"), cluster=id) -->
  
  <!-- H3 <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position_red), data=filter(main, mp_type_detailed=="moderate rebel"), cluster=id) -->
  
  <!-- H5 <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position_red), data=filter(main, mp_type_detailed=="extreme rebel"), cluster=id) -->
  
  <!-- H7<-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position_red), data=main, cluster=id) -->
  
  <!-- aa<-huxtable::huxreg('Loyalists' = H1, -->
                              <!--                      'All' = H7,  -->
                              <!--                      coefs = c("Party congruent"= "partycongruent")) %>% -->
  <!--   insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>% -->
  <!--   insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>% -->
  <!--   insert_row("Send order (reduced cat) FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>% -->
  <!--   set_top_border(4, 2:3, 1) %>% -->
  <!--   set_bottom_border(4, 2:3, 0) %>% -->
  <!--   set_bottom_border(6, 2:3, 1) %>% -->
  <!--   set_align('centre')  -->
  
  <!-- aa -->
  
  
  <!-- ``` -->
  
  Here are the model estimates when we break the sample down into all four types of case (loyalist, moderate rebel, extreme rebel and all cases):
  
  ```{r response-models-detailed, warning=FALSE, message=FALSE, echo = FALSE}

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




\newpage

To get a sense of how precise these estimates are, we provide coefficient plots which show 95\% confidence intervals. For ease of quoting numbers in the write-up, we also print a table which shows the numeric values of the CI bounds. 


```{r response-coefplot-prep, warning=FALSE, message=FALSE, echo = FALSE}

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


```

```{r response-coefplot-simple, fig.cap="Coefficient plot for MP responsiveness models", fig.width=6, fig.height=3, echo = FALSE}

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


```{r response-coefplot-detailed, fig.cap="Coefficient plot for MP responsiveness models", fig.width=6, fig.height=4, echo = FALSE}

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


H7_brexit <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), 
                      data=main %>%
                        filter(issue %in% c("customs", "singlem",
                                            "freedomm", "regulation", 
                                            "secondr")), 
                      cluster=id)


H7_nonbrexit <-lm_robust(manualresponse_exists_inc_post_count_nonresponse~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), 
                         data=main %>%
                           filter(!(issue %in% c("customs", "singlem",
                                                 "freedomm", "regulation", 
                                                 "secondr"))), 
                         cluster=id)

aa<-huxtable::huxreg('Brexit letters' = H7_brexit,
                     'Non-Brexit letters' = H7_nonbrexit, 
                     coefs = c("Party congruent"= "partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("Linear probability models of MP responsiveness for Brexit and non-Brexit letters, separately.")

label(aa) <- "tab:response-regs-by-brexit"


aa


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

```{r lmodel_asschecks, warning=FALSE, message=FALSE, echo = FALSE}

main <- main %>%
  mutate(emailonlyresponse = case_when(respondent_reported_letter_status==1 ~ 0,
                                       manualresponse_exists==1 ~ 1,
                                       TRUE ~ NA_real_
  ),
  has_response_info = as.numeric(!is.na(respondent_reported_letter_status)))
H1<-lm(has_response_info~ partycongruent, data=main)
H2<-lm_robust(has_response_info~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)
H3<-lm(respondent_reported_letter_status~ partycongruent, data=main)
H4<-lm_robust(respondent_reported_letter_status~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)
H5<-lm_robust(manualresponse_exists~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg('Letter Status Reported' = H2, 'Letter Response' = H4, 'Email Response' = H5,
                     coefs = c("Party congruent" = "partycongruent")) %>%
  insert_row("MP fixed effects", "yes", "yes", "yes", after=3, copy_cell_props = FALSE) %>%
  insert_row("position fixed effects", "yes", "yes", "yes", after=4, copy_cell_props = FALSE) %>%
  insert_row("send order fixed effects", "yes",  "yes", "yes", after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:4, 1) %>%
  set_bottom_border(4, 2:4, 0) %>%
  set_bottom_border(6, 2:4, 1) %>%
  set_align('centre')  %>%
  set_caption("Linear probability models of: whether constituent reports letter response status; MP letter response among cases where status is known; MP email response among cases where it is known.")

label(aa) <- "tab:response-regs-letters"


aa


```


\newpage 

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



main <- main %>% 
  mutate(partycongruent_neat = recode(partycongruent, 
                                      `0` = "Constituent-party not congruent",
                                      `1` = "Constituent-party congruent"))

p4 <- ggplot(main, aes(x = lr_mp_party)) + 
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

p4 

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


\newpage

### Regression analysis of text measures

Now we perform regression analysis to test whether MP vs party emphasis in MP response text varies by constituent-party congruence on the issue. The DV is the log ratio of MP vs party mentions. We focus on this DV as it was the one highlighted in the pre-analysis plan.

We do not standardise this DV at the moment as the exponentiated coefficient is directly interpretable in terms of proportional change in ratio of MP to party mentions. For example, the estimated coefficient of -0.016 can be interpreted as follows: when the constituent is congruent with the MP's party, the ratio of MP mentions to party mentions is 0.98 (i.e., $exp(-0.016) = 0.98$)) times what it would have been if the constituent was not congruent, all else equal (i.e., a very small change). 

```{r text-regs-detailed, echo = TRUE, warning = FALSE, message=FALSE, echo=FALSE}

H2<-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)

H4 <-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="moderate rebel"), cluster=id)

H6 <-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="extreme rebel"), cluster=id)

H8<-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg('Loyalists' = H2, 'Moderate rebels' = H4,
                     'Extreme rebels)' = H6,  
                     'All' = H8, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 4), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:5, 1) %>%
  set_bottom_border(4, 2:5, 0) %>%
  set_bottom_border(6, 2:5, 1) %>%
  set_align('centre')

aa


```

This is a simpler version of the table with models run on the loyalists subsample and the full sample only. 

```{r text-regs-simple, warning = FALSE, message=FALSE, echo=FALSE}

aa<-huxtable::huxreg('Full sample' = H8, 
                     'Loyalist MPS' = H2, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("OLS models of log ratio of MP vs party mentions in observed responses")

label(aa) <- "tab:text-regs-main"


aa


```

Now check results are similar when we classify loaylist MPs based on a vote-coding scheme which never code abstentions as rebellions. 

```{r text-regs-simple-abstain-nonreb, echo = TRUE, warning = FALSE, message=FALSE, echo=FALSE}

H2_abstain_nonreb<-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_abstain_nonreb=="loyalist"), cluster=id)

H8_abstain_nonreb<-lm_robust(lr_mp_party~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg('Full sample' = H8_abstain_nonreb, 
                     'Loyalist MPS' = H2_abstain_nonreb, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("OLS models of log ratio of MP vs party mentions in observed responses")

label(aa) <- "tab:text-regs-abstain-nonreb"


aa



```


\newpage

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

H2<-lm_robust(diff_mp_party_outliers_removed~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)

H4 <-lm_robust(diff_mp_party_outliers_removed~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="moderate rebel"), cluster=id)

H6 <-lm_robust(diff_mp_party_outliers_removed~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="extreme rebel"), cluster=id)

H8<-lm_robust(diff_mp_party_outliers_removed~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg('Loyalists' = H2, 'Moderate rebels' = H4,
                     'Extreme rebels)' = H6,  
                     'All' = H8, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 4), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:5, 1) %>%
  set_bottom_border(4, 2:5, 0) %>%
  set_bottom_border(6, 2:5, 1) %>%
  set_align('centre')

aa


```

```{r text-regs-alt1-simple, echo=FALSE, warning = FALSE, message=FALSE}

aa<-huxtable::huxreg('Full Sample' = H8, 
                     'Loyalist MPs' = H2, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("OLS models of  difference in number of MP mentions and party mentions, both standardised by overall number of words in the letter.")

label(aa) <- "tab:text-regs-diff"


aa


```



\newpage

As a further check, we estimate models where the DV is the log word lengtth of the letter.


```{r text-regs-alt2-detailed, echo=FALSE, warning = FALSE, message=FALSE}

H2<-lm_robust(I(log(word_total))~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="loyalist"), cluster=id)

H4 <-lm_robust(I(log(word_total)) ~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="moderate rebel"), cluster=id)

H6 <-lm_robust(I(log(word_total)) ~ partycongruent + factor(issue)*factor(position) + factor(onsconstid)+factor(send_position), data=filter(main, mp_type_detailed=="extreme rebel"), cluster=id)

H8<-lm_robust(I(log(word_total)) ~ partycongruent + factor(issue)*factor(position) + factor(onsconstid) + factor(send_position), data=main, cluster=id)

aa<-huxtable::huxreg('Loyalists' = H2, 'Moderate rebels' = H4,
                     'Extreme rebels)' = H6,  
                     'All' = H8, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 4), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 4), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:5, 1) %>%
  set_bottom_border(4, 2:5, 0) %>%
  set_bottom_border(6, 2:5, 1) %>%
  set_align('centre')

aa


```

```{r text-regs-alt2-simple, echo=FALSE, warning = FALSE, message=FALSE}

aa<-huxtable::huxreg('Full Sample' = H8, 
                     'Loyalist MPs' = H2, 
                     coefs = c("Party congruent"="partycongruent")) %>%
  insert_row("MP FE?", rep("yes", 2), after=3, copy_cell_props = FALSE) %>%
  insert_row("Position FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  insert_row("Send order FE?", rep("yes", 2), after=4, copy_cell_props = FALSE) %>%
  set_top_border(4, 2:3, 1) %>%
  set_bottom_border(4, 2:3, 0) %>%
  set_bottom_border(6, 2:3, 1) %>%
  set_align('centre') %>%
  set_caption("OLS models of log word length of MP response text.")

label(aa) <- "tab:text-regs-length"


aa


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


