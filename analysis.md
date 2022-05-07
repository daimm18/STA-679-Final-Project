---
title: "Analysis"
author: "Michelle Dai"
date: '2022-05-06'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r, results='hide', message=FALSE, warning=FALSE}
library(causaldata)
library(dbplyr)
library(ggdag)
library(tidyverse)
library(broom)
library(visdat)
data <- read.csv(file="marketing_campaign.csv")
```

```{r}
data$TotalPurchase <- data$NumWebPurchases+ data$NumCatalogPurchases+data$NumStorePurchases
data_sub <- data %>% select(Year_Birth, Education, Marital_Status, Income, Kidhome, Teenhome, Recency, Complain, NumDealsPurchases, TotalPurchase, NumWebVisitsMonth)
```

## Missing Data Imputation


```{r, message=FALSE, warning=FALSE}
vis_dat(data_sub) + labs(title = "Figure 1: Missing Data")
```


From Figure 1, we can see that there are 24 missing data in "Income", because we understand that "Income" might be a censored data. From the DAG we introduced in the previous section, we assume that our dataset exist other variables that could potentially explain part of the "Income", so "Income" is Missing at Random with all other variables in our dataset not missing. We are going to impute the missing data properly so that we lose minimal information on the group missing "Income".


```{r, warning=FALSE, message=FALSE}
library(mice)
library(MatchThem)
library(survey)
set.seed(1)
```


Here, I used 5 different models to predict "Income", and I used predictive mean matching. The approach is to calculate the predicted value of "Income" for all observations using the other variables, and by randomly selecting one of the non-missing values of "Income" among the sets of observations that have the closest predicted value for variable "Income" as the imputed value, and I believe the imputed value is the average value from these five models.


```{r, warning=FALSE, message=FALSE}
lm(Recency~Income, data = data_sub) %>%
  tidy()
data_sub_imp <- mice(data_sub, 5, "pmm", printFlag = FALSE)

data_sub_complete <- complete(data_sub_imp)

lm(Recency~Income, data = data_sub_complete) %>%
  tidy()
```


We can see from the two simple linear models fitted ($Recency=\beta_0 + \beta_1 *Income$) using the dataset before imputation and the dataset after imputation (averaged), although there are some differences between coefficients as we expected, they do not off much, so we expect our data imputation is reasonable at this stage.


```{r}
data_sub_complete <- data_sub_complete %>% 
  mutate(Education = ifelse(Education == "2n Cycle", 0, Education))%>% 
  mutate(Education = ifelse(Education == "Basic", 1, Education)) %>% 
  mutate(Education = ifelse(Education == "Graduation", 2, Education)) %>% 
  mutate(Education = ifelse(Education == "Master", 3, Education)) %>% 
  mutate(Education = ifelse(Education == "PhD", 4, Education)) %>% 
  mutate(Marital_Status = ifelse(Marital_Status == "Single", 0, Marital_Status)) %>% 
  mutate(Marital_Status = ifelse(Marital_Status == "Together", 1, Marital_Status))%>% 
  mutate(Marital_Status = ifelse(Marital_Status == "Married", 2, Marital_Status))%>% 
  mutate(Marital_Status = ifelse(Marital_Status == "Divorced", 3, Marital_Status))%>% 
  mutate(Marital_Status = ifelse(Marital_Status == "Widow", 4, Marital_Status))%>% 
  mutate(Marital_Status = ifelse(Marital_Status == "Alone", 5, Marital_Status))%>% 
  mutate(Marital_Status = ifelse(Marital_Status == "Absurd", 6, Marital_Status))%>% 
  mutate(Marital_Status = ifelse(Marital_Status == "YOLO", 7, Marital_Status))
  
data_continuous <- data_sub_complete

data_sub_complete <- data_sub_complete %>% mutate(NumWebVisitsMonth = ifelse(NumWebVisitsMonth>=6, 1, 0))
```


After imputation, I changed "Education" and "Marital_Status" from string categories to dummy variables (0, 1, 2, etc.). In order to prepare for the propensity score modeling in the next part, I set a threshold for "NumWebVisitsMonth", which "NumWebVisitsMonth" greater or equal to 6 will consider as frequent, and infrequent vice versa. The reason behind is that at cutoff point 6, we will have group frequent and infrequent at 1070 and 1170 respectively, which is the most balanced group we can get with two categories. Then we first analyze the outcome model with binary exposure, and then analyze the outcome model with continuous exposure.



## Propensity Score Model {.tabset}

### Introduction to Propensity Score Model


The propensity score is the probability of treatment assignment conditional on observed baseline characteristics.

The propensity score is the probability of treatment assignment conditional on observed baseline characteristics. In order to estimate the effects of treatments, interventions, and exposures (hereafter referred to as treatments), randomized controlled trials (RCTs) are considered the gold standard. In this way, the treatment status will not be confounded with measured or unmeasured baseline characteristics. Therefore, the effect of treatment on outcomes can be estimated by comparing outcomes directly between treated and untreated subjects. An observational study can be designed and analyzed using a propensity score in a way that mimics some of the characteristics of a randomized controlled trial. In particular, the propensity score is a balancing score: conditional on the propensity score, the distribution of observed baseline covariates will be similar between treated and untreated subjects. (Austin, 2011)



```{r, message=FALSE, warning=FALSE}
library(tidyverse)
library(dplyr)
library(broom)
library(smd)
library(gtsummary)
library(survey)

data_sub_complete %>%
  tbl_summary(
    by = NumWebVisitsMonth, 
    include = c(
      "Year_Birth", "Education", "Marital_Status", "Kidhome", "Teenhome", "Recency", "NumDealsPurchases")) %>%
  add_overall()
```


From the summary table above, we can see that most factors are unbalanced between different exposure groups. For example, costumers with no kid at home are less likely to visit online store (80% vs. 38%), and costumers with one kid at home are more likely to visit online store (20% vs. 59%).


We are going to fit two propensity score model, the first one is unweighted, to see if we don't manually balance the differences between different exposure groups, what outcome model will return us with. The second propensity score model will be fitted after we perform ATO weight, since I am interested in the population of individuals who reasonably could either visit webstore frequently or infrequently, I will be using ATO for estimand.


### Unweighted


```{r, results='hide', message=FALSE, warning=FALSE}
data_sub_complete <- 
  glm(NumWebVisitsMonth ~ Year_Birth + as.factor(Education) +as.factor(Marital_Status)+ Kidhome + Teenhome + Recency + NumDealsPurchases, 
    data = data_sub_complete,
    family = binomial()) %>%
  augment(type.predict = "response",
          data = data_sub_complete) # concatenation
data_sub_complete %>%
  select(NumWebVisitsMonth, .fitted)
```


```{r, message=FALSE, warning=FALSE}
ggplot(data_sub_complete, aes(x= .fitted, fill = as.factor(NumWebVisitsMonth))) + 
  geom_histogram(bins = 30) +geom_vline(xintercept = median(data_sub_complete[data_sub_complete$NumWebVisitsMonth == 0, ]$.fitted), linetype="twodash", 
                color = "red", size=1)+geom_vline(xintercept = median(data_sub_complete[data_sub_complete$NumWebVisitsMonth == 1, ]$.fitted), linetype="twodash", 
                color = "turquoise4", size=1)+
  labs(x="Propensity Score", y="Count", title= "Figure 2: Distribution of Propensity Score before Weighting")

#median(data_sub_complete[data_sub_complete$NumWebVisitsMonth == 0, ]$.fitted)
#median(data_sub_complete[data_sub_complete$NumWebVisitsMonth == 1, ]$.fitted)
```


We can see that before weighting, the median of the propensity score for webstore visit infrequent group (less than 6 times) is 0.31, and the median of propensity score for webstore visit frequent group is 0.68. Since the propensity score is the probability of receiving treatment, this makes sense as those who received it probably had a higher probability of frequent website visit.


```{r, message=FALSE, warning=FALSE}
library(rsample)
library(PSW)

#fit inverse probablity weight model
fit_ipw <- function(split, ...) {
  .df <- analysis(split)
  # fit propensity score model
  propensity_model <- glm(
    NumWebVisitsMonth ~ Year_Birth + as.factor(Education) +as.factor(Marital_Status)+ Kidhome + Teenhome + Recency + NumDealsPurchases,
    data = .df,
    family = binomial()
  )
  # calculate inverse probability weights
  .df <- propensity_model %>% 
    augment(type.predict = "response", data = .df)
  # outcome model 
  lm(TotalPurchase ~ NumWebVisitsMonth, data = .df) %>% 
    tidy()
}

# fit ipw model to bootstrapped samples
ipw_results <- bootstraps(data_sub_complete, 1000, apparent = TRUE) %>% 
  mutate(results = map(splits, fit_ipw))
```


```{r, message=FALSE, warning=FALSE}
# get t-statistic-based CIs
boot_estimate <- int_t(ipw_results, results) %>%
  filter(term == c("(Intercept)", "NumWebVisitsMonth"))
boot_estimate
```


After performing 1000 Bootstrap samples, keeping all other factors constant, on average, we are 95% confident that people who visit webstore more frequently (greater or equal to 6) are purchasing 5.91 to 4.77 times less than people who visit webstore infrequently.


The estimated relationship between total purchases and number of time visit webstore is $TotalPurchase=15.335473-5.350321*NumWebVisitsMonth$


### ATO Weighted


Understanding the inbalance exists between treatment groups, we are applying ATO weighting here to see the outcome model after ATO weighting balanced.


```{r, message=FALSE, warning=FALSE}
data_sub_complete <- glm(NumWebVisitsMonth ~ Year_Birth + as.factor(Education) +as.factor(Marital_Status)+ Kidhome + Teenhome + Recency + NumDealsPurchases,
                         data=data_sub_complete, family = binomial()) %>%
  augment(data_sub_complete, type.predict = "response") %>%
  mutate(wt_ato = (1 - NumWebVisitsMonth)* .fitted + NumWebVisitsMonth * (1 - .fitted))
```


```{r, message=FALSE, warning=FALSE}
des <- svydesign(
  ids = ~1,
  data =  data_sub_complete,
  weight = ~ wt_ato
)

des %>%
  tbl_svysummary(
    by = NumWebVisitsMonth, 
    include = c(
      "Year_Birth", "Education", "Marital_Status", "Kidhome", "Teenhome", "Recency", "NumDealsPurchases")) %>%
  add_overall()
```


```{r, message=FALSE, warning=FALSE}
for_plot <- data_sub_complete %>% distinct() %>%
  pivot_wider(names_from = NumWebVisitsMonth,
              values_from = .fitted,
              names_prefix = "NumWebVisitsMonth_p")
```


```{r, message=FALSE, warning=FALSE}
ggplot(for_plot) +
  geom_histogram(bins = 50,
                 aes(x = NumWebVisitsMonth_p1, 
                     weight = wt_ato),  
                 fill = "cornflower blue") +
  geom_histogram(bins = 50,
                 aes(x = NumWebVisitsMonth_p0,
                     weight = wt_ato,
                     y = - stat(count)),
                 fill = "orange") + 
  scale_y_continuous("Count", label = abs) + 
  scale_x_continuous("Propensity Score (ATO Weighted)") +
  geom_label(
    label = "Visit >= 6", 
    x = 0.1,
    y = 10,
  )+
    geom_label(
    label = "Visit < 6", 
    x = 0.1,
    y = -10,
  )+ labs(title= "Figure 3: Distribution of Propensity Score (ATO Weighted)")
```


From table above as well as Figure 3, we can see that the distribution of propensity score is more balanced between treatments after ATO weighting. Thus, the probability of assigning to either treatment group is approximately the same now.


```{r, message=FALSE, warning=FALSE}
ecdf_1 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 1) %>%
  arrange(Income) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))

ecdf_0 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 0) %>%
  arrange(Income) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))
```


```{r, message=FALSE, warning=FALSE}
ggplot(ecdf_1, aes(x = Income, y = cum_pct_ato, color = "Yes")) +
  geom_line() +
  geom_line(data = ecdf_0, 
            aes(x = Income, y = cum_pct_ato, color = "No")) + labs(x = "Income after ATO weighting", y = "(ATO Weighted) Percent <= x", title="Figure 4: ATO Weighted ECDF for Income", color = "NumWebVisitsMonth")
```


```{r, message=FALSE, warning=FALSE}
ecdf_1 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 1) %>%
  arrange(Year_Birth) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))

ecdf_0 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 0) %>%
  arrange(Year_Birth) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))
```


```{r, message=FALSE, warning=FALSE}
ggplot(ecdf_1, aes(x = Year_Birth, y = cum_pct_ato, color = "Yes")) +
  geom_line() +
  geom_line(data = ecdf_0, 
            aes(x = Year_Birth, y = cum_pct_ato, color = "No")) + labs(x = "Year_Birth after ATO weighting", y = "(ATO Weighted) Percent <= x", title="Figure 5: ATO Weighted ECDF for Year_Birth", color = "NumWebVisitsMonth")
```


```{r, message=FALSE, warning=FALSE}
ecdf_1 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 1) %>%
  arrange(Recency) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))

ecdf_0 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 0) %>%
  arrange(Recency) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))
```


```{r, message=FALSE, warning=FALSE}
ggplot(ecdf_1, aes(x = Recency, y = cum_pct_ato, color = "Yes")) +
  geom_line() +
  geom_line(data = ecdf_0, 
            aes(x = Recency, y = cum_pct_ato, color = "No")) + labs(x = "Recency after ATO weighting", y = "(ATO Weighted) Percent <= x", title="Figure 6: ATO Weighted ECDF for Recency", color = "NumWebVisitsMonth")
```


```{r, message=FALSE, warning=FALSE}
ecdf_1 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 1) %>%
  arrange(Kidhome) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))

ecdf_0 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 0) %>%
  arrange(Kidhome) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))
```


```{r, message=FALSE, warning=FALSE}
ggplot(ecdf_1, aes(x = Kidhome, y = cum_pct_ato, color = "Yes")) +
  geom_line() +
  geom_line(data = ecdf_0, 
            aes(x = Kidhome, y = cum_pct_ato, color = "No")) + labs(x = "Kidhome after ATO weighting", y = "(ATO Weighted) Percent <= x", title="Figure 7: ATO Weighted ECDF for Kidhome", color = "NumWebVisitsMonth")
```


```{r, message=FALSE, warning=FALSE}
ecdf_1 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 1) %>%
  arrange(Teenhome) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))

ecdf_0 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 0) %>%
  arrange(Teenhome) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))
```


```{r, message=FALSE, warning=FALSE}
ggplot(ecdf_1, aes(x = Teenhome, y = cum_pct_ato, color = "Yes")) +
  geom_line() +
  geom_line(data = ecdf_0, 
            aes(x = Teenhome, y = cum_pct_ato, color = "No")) + labs(x = "Teenhome after ATO weighting", y = "(ATO Weighted) Percent <= x", title="Figure 8: ATO Weighted ECDF for Teenhome", color = "NumWebVisitsMonth")
```


```{r, message=FALSE, warning=FALSE}
ecdf_1 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 1) %>%
  arrange(NumDealsPurchases) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))

ecdf_0 <- data_sub_complete %>%
  filter(NumWebVisitsMonth == 0) %>%
  arrange(NumDealsPurchases) %>%
  mutate(cum_pct_ato = cumsum(wt_ato) / sum(wt_ato))
```


```{r, message=FALSE, warning=FALSE}
ggplot(ecdf_1, aes(x = NumDealsPurchases, y = cum_pct_ato, color = "Yes")) +
  geom_line() +
  geom_line(data = ecdf_0, 
            aes(x = NumDealsPurchases, y = cum_pct_ato, color = "No")) + labs(x = "NumDealsPurchases after ATO weighting", y = "(ATO Weighted) Percent <= x", title="Figure 9: ATO Weighted ECDF for NumDealsPurchases", color = "NumWebVisitsMonth")
```


In Figure 4-9, we checked the eCDF plot for all continuous variables (the latter ones are potentially continuous although the range of values this dataset took is not large). We can see that the eCDF plots for two groups match well as they mostly overlap with each other after ATO weighting.


```{r, message=FALSE, warning=FALSE}
smds <- data_sub_complete %>%
  summarise(
    across(
      c("Year_Birth", "Education", "Kidhome", "Teenhome", "Recency", "NumDealsPurchases"),
      list(
        unweighted = ~smd(.x, NumWebVisitsMonth)$estimate,
        ato.weighted = ~smd(.x, NumWebVisitsMonth, w = wt_ato)$estimate
      )
    )
  )


smds %>%
  pivot_longer(
    everything(),
    values_to = "SMD",
    names_to = c("variable", "Method"),
    names_pattern = "(.*)_(.*)") %>%
  arrange(desc(Method), abs(SMD)) %>%
  mutate(variable = fct_inorder(variable)) -> for_plot
```


```{r, message=FALSE, warning=FALSE}
ggplot(for_plot, aes(x = abs(SMD), y=variable, group = Method, color= Method))+ labs(x = "Standarized Mean Difference", y="Variables", title = "Figure 10: Love Plot") +
  geom_line(orientation = "y") +
  geom_point() +
  geom_vline(xintercept = 0.1, lty = 2, color = "black", size =0.1)
```


From Figure 10, we can see that before ATO weights adjusted, "KidHome" and "NumDealsPurchases" are heavily stressed as covariates, especially for "KidHome". The balance is improved for all variables after ATO weights adjusted (all variables are now below the threshold of 0.1 for absolute mean differences).


```{r, message=FALSE, warning=FALSE}
library(rsample)
library(PSW)
#fit inverse probablity weight model
fit_ipw <- function(split, ...) {
  .df <- analysis(split)
  # fit propensity score model
  propensity_model <- glm(
    NumWebVisitsMonth ~ Year_Birth + as.factor(Education) +as.factor(Marital_Status)+ Kidhome + Teenhome + Recency + NumDealsPurchases,
    data = .df,
    family = binomial()
  )
  # calculate inverse probability weights
  .df <- propensity_model %>% 
    augment(type.predict = "response", data = .df) %>% 
    mutate(wt_ato = (1 - NumWebVisitsMonth)* .fitted + NumWebVisitsMonth * (1 - .fitted))
  # fit correctly bootsrapped ipw model
  # outcome model 
  lm(TotalPurchase ~ NumWebVisitsMonth, data = .df, weights = wt_ato) %>% 
    tidy()
}

# fit ipw model to bootstrapped samples
ipw_results <- bootstraps(data_sub_complete, 1000, apparent = TRUE) %>% 
  mutate(results = map(splits, fit_ipw))
```


```{r, message=FALSE, warning=FALSE}
# get t-statistic-based CIs
boot_estimate <- int_t(ipw_results, results) %>%
  filter(term == c("(Intercept)", "NumWebVisitsMonth"))
boot_estimate
```


After we are certain that the probability of assigning to either treatment group is approximately the same, we perform 1000 Bootstrap samples, by keeping all other factors constant, on average, we are 95% confident that people who visit webstore more frequently (greater or equal to 6) are purchasing 3.56 to -2.48 times less than people who visit webstore infrequently.


The estimated relationship between total purchases and number of time visit webstore is $TotalPurchase=13.938669-3.029384*NumWebVisitsMonth$.


### Continuous Exposure IPW


We consider the potential bias we add to the outcome model with setting up a cutoff point for exposure group. Thus, we consider continuous exposure in this part trying to capture the true relationship between exposure and outcome.


Here, I have used inverse propensity weights (IPW), which provides an estimation of ATE weighting as in binary treatment group, which makes sense as we don't have enough information to choose either group in this case since our treatment is continuous.


```{r}
# the numerator
mod_num <- lm(NumWebVisitsMonth ~ 1, data = data_continuous)

num <- dnorm(x = data_continuous$NumWebVisitsMonth, # treatment 
             mean = fitted.values(mod_num), # fitted values
             sd = summary(mod_num)$sigma) # model sigma

mod_den <- lm(NumWebVisitsMonth ~ Year_Birth + as.factor(Education) +as.factor(Marital_Status)+ Kidhome + Teenhome + Recency + NumDealsPurchases, data = data_continuous)

den <- dnorm(x = data_continuous$NumWebVisitsMonth, # treatment variable
             mean = fitted.values(mod_den), # fitted values
             sd = summary(mod_den)$sigma)

data_continuous <- data_continuous %>%
  mutate(ipw_s = num/den)

#summary(data_continuous$ipw_s)
```


```{r}
ggplot(data_continuous, aes(ipw_s)) +
  geom_density(col = "#E69F00", fill = "#E69F0095") + 
  scale_x_log10()  + 
  xlab("Stabilized Weights")+ geom_vline(xintercept = median(data_continuous$ipw_s), linetype="twodash", color = "red", size=0.7) +
  labs(x="Stabilized Weights", y="Density", title= "Figure 10: Distribution of Weights after IPW")
```


We can see that after IPW the median of weights approximately 1 (0.8). Therefore we believe that we have tackled with existence of extreme weights (the problem that will result in large CI) (McGowan, 2022).


```{r, message=FALSE, warning=FALSE}
library(rsample)
library(PSW)
#fit inverse probablity weight model
fit_ipw <- function(split, ...) {
  .df <- analysis(split)
  # fit propensity score model
  mod_num <- lm(NumWebVisitsMonth ~ 1, data = .df)
  num <- dnorm(x = .df$NumWebVisitsMonth, # treatment 
             mean = fitted.values(mod_num), # fitted values
             sd = summary(mod_num)$sigma) # model sigma
  mod_den <- lm(NumWebVisitsMonth ~ Year_Birth + as.factor(Education) +as.factor(Marital_Status)+ Kidhome + Teenhome + Recency + NumDealsPurchases, data = .df)
  den <- dnorm(x = .df$NumWebVisitsMonth, # treatment variable
             mean = fitted.values(mod_den), # fitted values
             sd = summary(mod_den)$sigma)

  .df <- .df %>%
    mutate(ipw_s = num/den)

  # fit correctly bootsrapped ipw model
  # outcome model 
  lm(TotalPurchase ~ NumWebVisitsMonth, data = .df) %>% 
    tidy()
}

# fit ipw model to bootstrapped samples
ipw_results <- bootstraps(data_continuous, 1000, apparent = TRUE) %>% 
  mutate(results = map(splits, fit_ipw))
```


```{r, message=FALSE, warning=FALSE}
# get t-statistic-based CIs
boot_estimate <- int_t(ipw_results, results) %>%
  filter(term == c("(Intercept)", "NumWebVisitsMonth"))
boot_estimate
```


After we are certain that the probability of assigning to either treatment group is approximately the same, we performing 1000 Bootstrap samples, by keeping all other factors constant, on average, we are 95% confident that for every additional increase in number of webstore visits, the number of total purchases will decrease by 1.36 to 1.16.


The estimated relationship between total purchases and number of time visit webstore is $TotalPurchase=19.26304-1.26591*NumWebVisitsMonth$.



## Sensitivity Analysis {.tabset}


In this study, since "NumWebVisitsMonth" did not provide information of month year this study was collected on, so the impact of people shop during Black Friday and regular season are drastically different. Thus, an event-related bias is introduced into our study. Let's assume we are concerned with residual confounding of season change on shopping behavior. And we assume that this residual confounding has smd (estimated difference in standardized means between the unmeasured confounder in the exposed population and unexposed population) of 0.1, and outcome association (the estimated association between the unmeasured confounder and outcome) of 1.5.


### Unweighted


```{r, message=FALSE, warning=FALSE}
library(tipr)

adjust_coef(effect = -5.350321, smd = 0.1, outcome_association = 1.5)

```


When we plugging in our point estimate from 1000 Bootstrap samples into the equation, we see an adjusted effect of -5.500321. Thus if we see an confounder like this (event-related), we would expect our total purchases to decrease by 5.50 for people who visit webstore more frequently compared with people who visit less frequently (less than 6) when keeping all other factors constant. 


### ATO Weighted


```{r, message=FALSE, warning=FALSE}
library(tipr)

adjust_coef(effect = -3.029384, smd = 0.1, outcome_association = 1.5)

```


When we plugging in our point estimate from 1000 Bootstrap samples into the equation, we see an adjusted effect of -3.179384. Thus if we see an confounder like this (event-related), we would expect our total purchases to decrease by 3.18 for people who visit webstore more frequently compared with people who visit less frequently (less than 6) when keeping all other factors constant. 

### Continuous Exposure


```{r, message=FALSE, warning=FALSE}
library(tipr)

adjust_coef(effect = -1.26591, smd = 0.1, outcome_association = 1.5)
```


When we plugging in our point estimate from 1000 Bootstrap samples into the equation, we see an adjusted effect of -1.41591. Thus if we see an confounder like this (event-related), we would expect our total purchases to decrease by 1.42 for every additional webstore visit when keeping all other factors constant.



## Conclusion {.tabset}


### Unweighted


With our unweighted model, by keeping all other factors constant, on average, people who visit online store more frequently (greater or equal to 6) shop 5.91 to 4.77 times less than people who visit online store less frequently, and we predict that people who visit online store more frequently had total purchases of 9.99 times on average, and we expect this number to be 9.84 times on average after considering unincluded confounder on exposure.


### ATO Weighted


With our ATO weighted model, by keeping all other factors constant, on average, people who visit online store more frequently (greater or equal to 6) shop 3.56 to 2.48 times less than people who visit online store less frequently, and we predict that people who visit online store more frequently had total purchases of 10.91 times on average, and we expect this number to be 10.76 times after considering unincluded confounder on exposure.


### Continuous Exposure


With our ATO weighted model, by keeping all other factors constant, on average, for every additional increase in number of webstore visits, the number of total purchases will decrease by 1.36 to 1.16. So that for example if we visit webstore 6 times within a month, we expect to purchase 11.67 times on average in total, and we expect this number to be 10.77 times after considering unincluded confounder on exposure.

