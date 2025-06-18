
set.seed(1234)
current_mps_n <- 102 
cases_per_issue <- c(26, 29, 33, 45, 45, 20, 37, 34, 43)

invlogit<-function (x) 1/(1+exp(-x))

fake.dat<-function(mp_int, beta1=-.2, cases_per_issue, effect_sd=.2){
    # mp_letter_matrix: a data frame where ncol=number of issues and nrow=number of mps. Values in the 
    n_issue<-ncol(mp_int[,!names(mp_int)=="mp_id"])
    n_mp<-nrow(mp_int)
    cases_per_issue_df<-data.frame(variable=paste0("issue_", 1:n_issue), n_cases=cases_per_issue, stringsAsFactors = FALSE)
    
    names(mp_int)[!names(mp_int)=="mp_id"]<-paste0("issue_", 1:ncol(mp_int[,!names(mp_int)=="mp_id"]))
    
mp_int<-mp_int %>%
  mutate(mp_dissidence=rnorm(nrow(mp_int), 0, .5))
    
mp_int_long<-mp_int %>%
      gather(variable, mp_intercept, -mp_id, -mp_dissidence) %>%
      mutate(mp_party_agree=rbinom(nrow(.), 1, invlogit(log(.8/.2) - mp_dissidence))) # does mp agree with the party - we don't control this - assume average 80% agreement between MP and party - mp_dissidence controls the within MP cross issue correlation in disagreement with party
    
    
mp_letter_long<- bind_rows(mutate(mp_int_long, voter_agree_mp=1), mutate(mp_int_long, voter_agree_mp=0)) %>%
      mutate(voter_agree_party=as.numeric(voter_agree_mp==mp_party_agree)) %>% # voters agrees with party iff (voter agrees with mp & mp agrees with party) | (voter disagrees with mp & mp disagrees with party )  i.e. if voter agreement with party == mp agreement with party
      mutate(p=invlogit(mp_intercept) + beta1*voter_agree_party,
             y=rnorm(nrow(.), p, effect_sd),
             bin_p = p,
             bin_p = ifelse(bin_p<0, 0, ifelse(bin_p>1, 1, bin_p)),
             y_bin = rbinom(nrow(.), 1, bin_p),
             f_mp_id = as.character(mp_id)
      )
    
    # then select number of cases to represent what we actually have
    
    my_subset<- function (x){
      sample_n(x, x$n_cases[1])
    }
    mp_letter_long_red<- mp_letter_long %>%
      dplyr::select(mp_id, variable) %>%
      group_by(variable) %>%
      left_join(cases_per_issue_df, by="variable") %>%
      nest() %>%
      mutate(data=map(data, my_subset)) %>%
      unnest(data) %>%
      inner_join(mp_letter_long, by=c("variable", "mp_id"), relationship = "many-to-many")
    
    return(mp_letter_long_red)
  }

make_mp_effects<-function(n_mp, n_issues, corr_matrix, sds, mp_average_mean){
  #' Make mp effect for baseline response to email
  #'
  #' mp effects made up of overall MP average propensity to mention own position 
  #' +
  #' correlated variation from this average effect by issue
  #' Units are logit(p) 
  
  #' @param n_mp Number of mps
  #' @param n_issues Number of issues
  #' @param corr_matrix Correlation matrix (dimension [n_issue + 1, n_issue + 1] where the +1 is the MP intercept) indicating the correlation in mp response type to each issues (where correlations with first value are correlation with the intercept - i.e. average response for that MP)
  #' @param sds Standard deviations of MP intercepts on each issue from the average from that MP (where the first sd is the sd of the MP intercepts)
  #' @param mp_average_mean Global average of MP effects
  
  cov_matrix<-diag(sds) %*% corr_matrix %*% diag(sds)
  Chol<-chol(cov_matrix)
  uncorrelated_dat<-matrix(rnorm(n_mp*(n_issues+1), 0, 1), nrow=n_issues+1)
  correlated_dat<-t(uncorrelated_dat) %*% Chol
  mp_average <- correlated_dat[,1] + mp_average_mean
  mp_int <- data.frame(mp_average + correlated_dat[,2:ncol(correlated_dat)]) %>%
    rowid_to_column("mp_id")
  names(mp_int)[!names(mp_int)=="mp_id"]<-paste0("issue_", 1:ncol(mp_int[,!names(mp_int)=="mp_id"]))
  
  mp_int
}


Co<-matrix(nrow=10, ncol=10, rep(.5, 100))
Co[1,]<-rep(.7, ncol(Co))
Co[,1]<-rep(.7, ncol(Co))
diag(Co)<-1

mp_int<-make_mp_effects(current_mps_n, 9, Co, rep(1, 10), .5)




f.pow<-function(n_mp, n_issues, n.sims=100, beta1=-.1, corr_matrix, sds, mp_average_mean, cases_per_issue, effect_sd=.2){
  lm.signif.bin.b1<-rep(NA, n.sims) # binary power beta1
  
  lm.signif.b1<-rep(NA, n.sims) # continious power beta1
  
  mp_int<-make_mp_effects(n_mp, n_issues, corr_matrix, sds, mp_average_mean)
  for (u in 1:n.sims){
    fake<-fake.dat(mp_int, beta1=beta1, cases_per_issue = cases_per_issue, effect_sd=effect_sd)
    #lm.power<-lm(y~voter.agree.mp+voter.agree.party+voter.agree.party*voter.agree.mp+f.mp.index, data=fake)
    
    #binary case
    lm.power<-lm(y_bin~voter_agree_party + f_mp_id, data=fake)
    theta.hat<-summary(lm.power)$coefficients[,1]["voter_agree_party"]
    theta.se<-summary(lm.power)$coefficients[,2]["voter_agree_party"]
    lm.signif.bin.b1[u]<-(abs(theta.hat)-2*theta.se)>0
    
    # continuous case
    lm.power1<-lm(y~voter_agree_party +f_mp_id, data=fake)
    theta.hat<-summary(lm.power1)$coefficients[,1]["voter_agree_party"]
    theta.se<-summary(lm.power1)$coefficients[,2]["voter_agree_party"]
    lm.signif.b1[u]<-(abs(theta.hat)-2*theta.se)>0
  }
  return(data.frame(bin_b1=mean(lm.signif.bin.b1), cont_b1=mean(lm.signif.b1)))
}


  
pa.res<-NULL


for (beta1 in seq(0, .15, .01)){
  
  power.anal.this<-f.pow(n_mp=current_mps_n, n_issue=9, n.sims=1000, beta1=beta1, corr_matrix = Co, sds=rep(.2,10), mp_average_mean=.5, cases_per_issue = cases_per_issue, effect_sd=.2)
  
  pa.df<-data.frame(power=t(power.anal.this))
  #pa.df<-data.frame(apply(pa.df, 2, unlist))
  pa.df<-pa.df %>%
    rownames_to_column("variable")
  new.dat<-cbind(data.frame(beta1=as.character(beta1), pa.df))
  if (is.null(pa.res)){
    pa.res<-new.dat
  } else {
    pa.res<-rbind(pa.res, new.dat)
  }
  
}
pa.res<-pa.res %>%
  separate(variable, c("type", "variable"), "_")

p1<-pa.res %>%
  mutate(effect_size = as.numeric(as.character(beta1))) %>%
  filter(type=="cont") %>%
  ggplot( aes(effect_size, power))+
  geom_line()+
  geom_hline(yintercept = .8)+
  ggtitle("Power Analysis: Continuous")

p2<-pa.res %>%
  mutate(effect_size = as.numeric(as.character(beta1))) %>%
  filter(type=="bin") %>%
  ggplot( aes(effect_size, power))+
  geom_line()+
  geom_hline(yintercept = .8)+
  ggtitle("Power Analysis: Binary")
