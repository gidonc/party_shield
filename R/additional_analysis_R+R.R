

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
