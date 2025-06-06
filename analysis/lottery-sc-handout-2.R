## -----------------------------------------------------------------------------------------------------------------
load(file="../data/lottery_lang.Rda")


## -----------------------------------------------------------------------------------------------------------------
#| output: false
library(tidyverse)
library(knitr)
library(tidysynth)
## If augsynth has not yet been installed, run the following lines:
# library(devtools)
# devtools::install_github("ebenmichael/augsynth")
library(augsynth)
# install.packages("gsynth") # Run once if not yet installed
library(gsynth)


## -----------------------------------------------------------------------------------------------------------------
### First, create a dataset with a treatment indicator:
OH_data <- lang_0624 %>% 
  dplyr::filter(type2 %in% c("Ohio","Non-Lottery State")) %>%
  mutate(treated=if_else(state=="OH" & rel_week >= 0,1,0))
### Then, run the augsynth function:
OH_as <- augsynth(form=people_fully_vaccinated_per_hundred~treated, # outcome~treatment
                  unit=stateF, # units, as a factor variable
                  time=week, # time period variable
                  data=OH_data, # data set
                  progfunc="None", # fits without any outcome model
                  scm=TRUE, # fits with SC weighting
                  fixedeff=FALSE) # fits without de-meaning/intercepts
## Prints average ATT estimate in post-intervention periods:
OH_as
## Prints estimate for each time period with conformal inference CI:
summary(OH_as)


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference, Observed – Synthetic Ohio, in percent fully vaccinated rate by week (centered at lottery announcement date, May 12, 2021)."
#| fig-alt: "Line plot that fluctuates near 0 from Week -17 to -6, is at -0.6 at Week -5, between -0.25 and 0.3 from Weeks -4 to -2, and then decreases steadily to -1.25 at Week 3 and stays near there until Week 6."

## The original SC fit from the tidysynth package:
load(file="synth_ohio.Rda") ## Loaded from previous analysis handout

synth_ohio %>% plot_differences() +
  labs(x="Weeks from Lottery Announcement",
       y="Difference in Percent Fully Vaccinated, Observed–Synthetic",
       title=NULL) +
  theme_bw()


## -----------------------------------------------------------------------------------------------------------------
## The new SC fit from the augsynth package:
#| fig-cap: "Time series of the difference, Observed – Synthetic Ohio, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that is the same as the previous plot, except the x-axis goes from 0 to 25, with a vertical line at 19."
plot(OH_as, 
     inf=FALSE) # Suppresses plotting of conformal inference CIs


## -----------------------------------------------------------------------------------------------------------------
### Run the augsynth fit with a ridge outcome model and SC
OH_as_r <- augsynth(form=people_fully_vaccinated_per_hundred~treated, # outcome~treatment
                  unit=stateF, # units, as a factor variable
                  time=week, # time period variable
                  data=OH_data, # data set
                  progfunc="Ridge", # fits with ridge outcome model
                  scm=TRUE, # fits with SC weighting
                  fixedeff=FALSE) # fits without de-meaning/intercepts
## Prints average ATT estimate in post-intervention periods:
OH_as_r


## -----------------------------------------------------------------------------------------------------------------
## The ridge-adjusted SC fit from the augsynth package:
#| fig-cap: "Time series of the difference, Observed – Ridge-adjusted Synthetic Ohio, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that is very similar to the previous plot."
plot(OH_as_r, 
     inf=FALSE) # Suppresses plotting of conformal inference CIs


## -----------------------------------------------------------------------------------------------------------------
### Run the augsynth fit with fixed effects and no outcome model
OH_as_fe <- augsynth(form=people_fully_vaccinated_per_hundred~treated, # outcome~treatment
                  unit=stateF, # units, as a factor variable
                  time=week, # time period variable
                  data=OH_data, # data set
                  progfunc="None", # fits without outcome model
                  scm=TRUE, # fits with SC weighting
                  fixedeff=TRUE) # fits with de-meaning/intercepts
## Prints average ATT estimate in post-intervention periods:
OH_as_fe


## -----------------------------------------------------------------------------------------------------------------
## The de-meaned SC fit from the augsynth package:
#| fig-cap: "Time series of the difference, Observed – de-meaned Synthetic Ohio, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates near 0, never exceeding 0.5 in either direction, from Week 0 to 18, and then steadily decreases to around -2 in Week 22 and stays around there through Week 25."
plot(OH_as_fe, 
     inf=FALSE) # Suppresses plotting of conformal inference CIs


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Plot of percent fully vaccinated rates by U.S. state, Jan.-Sept. 2021"
#| fig-alt: "Line plot with several Other Lottery States and Non-Lottery States, as well as three focused states: Ohio, New Mexico, and Maine. Maine is in the middle of the set of lines, while New Mexico is among the highest early on, and Maine is among the highest in May/June."
ggplot(data=lang_0624,
       mapping=aes(group=state, linetype=type2, color=type2,
                   alpha=type2, linewidth=type2,
                   x=last_day,y=people_fully_vaccinated_per_hundred)) +
  scale_alpha_manual(name=NULL, breaks=c("Ohio","New Mexico","Maine",
                                         "Other Lottery State",
                                         "Non-Lottery State"),
                     values=c(1,1,1,0.5,0.8)) + 
  scale_linewidth_manual(name=NULL, breaks=c("Ohio","New Mexico","Maine",
                                         "Other Lottery State",
                                         "Non-Lottery State"),
                         values=c(1.3,1.3,1.3,1,1)) +
  geom_line() + theme_bw() +
  geom_vline(data=lang_0624 %>% 
               dplyr::filter(type2 %in% c("Ohio","New Mexico","Maine"),
                                              rel_week==0),
             mapping=aes(xintercept=lott_date, group=type2, color=type2),
             linetype="dotted") +
  labs(x="Day (2021)", y="Percent Fully Vaccinated",
       linetype=NULL,color=NULL)


## -----------------------------------------------------------------------------------------------------------------
## Create data set for New Mexico as treated unit:
NM_data <- lang_0624 %>% 
  dplyr::filter(type2 %in% c("New Mexico","Non-Lottery State")) %>%
  mutate(treated=if_else(state=="NM" & rel_week >= 0,1,0))
## Create data set for Maine as treated unit:
ME_data <- lang_0624 %>% 
  dplyr::filter(type2 %in% c("Maine","Non-Lottery State")) %>%
  mutate(treated=if_else(state=="ME" & rel_week >= 0,1,0))

## Fit standard SCs using augsynth:
NM_as <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=NM_data,
                  progfunc="None", scm=TRUE, fixedeff=FALSE)
ME_as <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="None", scm=TRUE, fixedeff=FALSE)


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference, Observed – Synthetic New Mexico, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates greatly, from around -0.3 to 2.5, until a vertical line at Week 22, and then remains in the 0.4 to 1.2 range through Week 25."
plot(NM_as, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Create a data set with the weights and sort largest to smallest in absolute value.
NM_as_w <- tibble(State=rownames(NM_as$weights),
                  Weight=NM_as$weights[,1]) %>%
  arrange(desc(abs(Weight)))
## Print results:
NM_as_w


## -----------------------------------------------------------------------------------------------------------------
## Fit de-meaned SC using augsynth:
NM_as_fe <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=NM_data,
                  progfunc="None", scm=TRUE, fixedeff=TRUE)


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference, Observed – De-meaned Synthetic New Mexico, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates from around -1 to 1, until a vertical line at Week 22, and then remains in the 0.7 to 1.5 range through Week 25."
plot(NM_as_fe, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Create a data set with the weights and sort largest to smallest in absolute value.
NM_as_fe_w <- tibble(State=rownames(NM_as_fe$weights),
                  Weight=NM_as_fe$weights[,1]) %>%
  arrange(desc(abs(Weight)))
## Print results:
NM_as_fe_w


## -----------------------------------------------------------------------------------------------------------------
## Fit ridge-adjusted SC using augsynth:
NM_as_r <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=NM_data,
                  progfunc="Ridge", scm=TRUE, fixedeff=FALSE)


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference, Observed – Ridge-adjusted Synthetic New Mexico, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates from around -0.05 to 0.05, until a vertical line at Week 22, and then rises quickly to just above 1 at Week 24 and remains there through Week 25."
plot(NM_as_r, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Create a data set with the weights and sort largest to smallest in absolute value.
NM_as_r_w <- tibble(State=rownames(NM_as_r$weights),
                  Weight=NM_as_r$weights[,1]) %>%
  arrange(desc(abs(Weight)))
## Print results:
NM_as_r_w


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference, Observed – Synthetic Maine, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates greatly, from around -2 to 1, until a vertical line at Week 24, and then is around -0.25 at Week 25."
plot(ME_as, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Fit de-meaned SC using augsynth:
ME_as_fe <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="None", scm=TRUE, fixedeff=TRUE)


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference, Observed – De-meaned Synthetic Maine, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that looks very similar to the previous plot."
plot(ME_as_fe, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Fit ridge-adjusted SC using augsynth:
ME_as_r <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="Ridge", scm=TRUE, fixedeff=FALSE)


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference, Observed – Ridge-adjusted Synthetic Maine, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates greatly, from around -1.3 to 1, with peaks getting smaller as Week goes toward 22, is at -0.25 in Week 23, -0.6 in Week 24 and -0.5 in Week 25."
plot(ME_as_r, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Fit de-meaned and ridge-adjusted SC using augsynth:
ME_as_r_fe <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="Ridge", scm=TRUE, fixedeff=TRUE)


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference, Observed – De-meaned and Ridge-adjusted Synthetic Maine, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that is at almost exactly 0 through Week 23, and then around -0.9 and -0.6 in Weeks 24 and 25, respectively."
plot(ME_as_r_fe, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Check the weights:
ME_as_r_fe$weights


## -----------------------------------------------------------------------------------------------------------------
ME_gsc <- gsynth(formula=people_fully_vaccinated_per_hundred~treated, 
                 data=ME_data, 
                 X=NULL, 
                 index=c("stateF","week"), force="two-way", 
                 r=0, CV=TRUE)


## -----------------------------------------------------------------------------------------------------------------
ME_gsc$wgt.implied


## -----------------------------------------------------------------------------------------------------------------
#| warning: false
#| fig-cap: "Time series of the difference, Observed – Generalized Synthetic Maine, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates greatly, from around -1 to 1, until a vertical line at Week 24, and then is around -0.2 and 0 at Weeks 24 and 25, respectively."
plot(ME_gsc, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Then we can fit the model using the GSYN option for progfunc:
ME_gsynth <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="GSYN")


## -----------------------------------------------------------------------------------------------------------------
#| warning: false
#| fig-cap: "Time series of the difference, Observed – Generalized Synthetic Maine, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates greatly, from around -2 to 1, until a vertical line at Week 24, and then is around -1.5 and -1.2 at Weeks 24 and 25, respectively."
plot(ME_gsynth, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Creating lag variable:
ME_data <- 
    ME_data %>%
    group_by(state) %>%
    mutate(lag.pfv = dplyr::lag(people_fully_vaccinated_per_hundred, 
                                n = 1, default = NA))
ME_gsc_cov <- gsynth(formula=people_fully_vaccinated_per_hundred~treated+
                   new_case_per_million+new_death_per_million+lag.pfv, 
                 data=ME_data %>% dplyr::filter(!is.na(lag.pfv)),
                 index=c("stateF","week"), force="two-way", 
                 r=0, CV=TRUE)


## -----------------------------------------------------------------------------------------------------------------
#| warning: false
#| fig-cap: "Time series of the difference, Observed – Generalized Synthetic Maine, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates greatly, from around -1 to 0.75, until a vertical line at Week 24, and then is around -0.2 and 0.1 at Weeks 24 and 25, respectively."
plot(ME_gsc_cov, 
     inf=FALSE)


## -----------------------------------------------------------------------------------------------------------------
## Create data set with indicator for treatment
Mult_data <- lang_0624 %>% 
  mutate(treated=if_else(!lottery,0,if_else(rel_week>=0,1,0)))
## Fit augmented SC on full data set:
Mult_as <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                    unit=stateF,
                    time=week,
                    data=Mult_data)
## Print results and summary:
Mult_as
summary(Mult_as)


## -----------------------------------------------------------------------------------------------------------------
#| fig-cap: "Time series of the difference in fully vaccinated percentage using staggered adoption synthetic control estimates for treated states, by time relative to intervention."
#| fig-alt: "Line plot with lines for 15 states, generally fluctuating between -1.25 and 1.25 prior to Week 0, and then expanding outward to a range of around -3 (New York) to 2.4 (Oregon) in Week 1. A darker average line is fairly close to 0 throughout"
plot(Mult_as)

