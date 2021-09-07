##Still to be done:
###Run one or two more models
###Evaluate all residuals plots
###Interpret models
###Select a model based on some metric

#LIBRARIES
library(tidyverse)
library(GGally)
library(psych)
library(stats)

#RANDOM SEED
set.seed(210904)

#IMPORT
raw <- read.csv("C:/Users/dmosc/OneDrive/Documents/academic/CUNY SPS/DATA621/HW1/moneyball-training-data.csv")

#DATA EXPLORATION
#Center, shape, spread.

summary(raw)

#From summary, we can see that some variables show a potentially large number of missing values, especially `TEAM_BATTING_HBP` and `TEAM_BASERUN_CS`. Some variables show suspicious outliers, such as `TEAM_PITCHING_H`, `TEAM_PITCHING_BB`, `TEAM_PITCHING_SO`, and `TEAM_FIELDING_E`.

#Also from summary, we can see that some variables contain entries of zero that don't make sense in the context of a baseball season. These variables include `TARGET_WINS`, `TEAM_BATTING_3B`, `TEAM_BATTING_HR`, `TEAM_BATTING_BB`, `TEAM_BATTING_SO`, `TEAM_BASERUN_SB`, `TEAM_BASERUN_CS`, `TEAM_PITCHING_HR`, `TEAM_PITCHING_BB`, `TEAM_PITCHING_SO`. At least some of these entries we know to be erroneous. For example, the all-time minimum batting strikeouts for a team over a complete season was 308, achieved by the 1921 Cincinnati Reds (https://www.baseball-almanac.com/recbooks/rb_strike2.shtml).

#Investigating NAs more closely:

colMeans(is.na(raw))

#`TEAM_BATTING_HBP` is comprised of almost 92% missing values. This variable cannot provide much information to our model. `TEAM_BASERUN`CS` also displays a very high fraction of missing values, at 34%.

#Examining the shape and spread of each variable:

raw %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()

#Density plots for each variable reveals the highly skewed shapes of `TEAM_FIELDING_E`, `TEAM_PITCHING_BB`, `TEAM_PITCHING_H`, AND `TEAM_PITCHING_SO` as suggested by the numeric summary above. `TEAM_BASERUN_SB` and `TEAM_BATTING_3B` display moderate skewness. Other distributions appear roughly normal or bimodal. Let's look more closely at each of the highly skewed variables:

ggplot(data = raw, aes(x = TEAM_FIELDING_E)) +
  geom_histogram()

#A very small number of entries exceed 1000. Excluding these, the distribution is moderately skewed right.

ggplot(data = raw, aes(x = TEAM_PITCHING_BB)) +
  geom_histogram()

#The distribution appears normal except for a very small number of entries greater than 1000.

ggplot(data = raw, aes(x = TEAM_PITCHING_H)) +
  geom_histogram()

#Let's look more closely at the region where most of this variable's values lie, [0,6000]:

raw %>%
  filter(TEAM_PITCHING_H < 6000) %>%
  ggplot(aes(x = TEAM_PITCHING_H)) +
  geom_histogram()

#The variable is skewed even within this narrower region surrounding the peak. This suggests possible data entry error for values greater than 3250.

ggplot(data = raw, aes(x = TEAM_PITCHING_SO)) +
  geom_histogram()

#Narrowing in:

raw %>%
  filter(TEAM_PITCHING_SO < 2500) %>%
  ggplot(aes(x = TEAM_PITCHING_SO)) +
  geom_histogram()

#The data appear roughly normal, except for a significant number of unrealistic zero-values. This suggests possible data entry error for values greater than or equal to 2000.

ggplot(data = raw, aes(x = TEAM_BASERUN_SB)) +
  geom_histogram()

#The shape of this data does not suggest any data entry errors, even though there exist a few extreme outliers.

ggplot(data = raw, aes(x = TEAM_BATTING_3B)) +
  geom_histogram()

#The shape of this data does not suggest any data entry errors, even though there exist a few extreme outliers.

#One assumption of linear models is that their explanatory variables be linearly related to the response variable. Let's examine this using scatterplots:

#First examine the `BATTING` variables:
pairs.panels(raw[,c(2:9)],
             method = 'pearson')

#Variables that fail even a roughly linear relationship are TEAM_BATTING_HR, TEAM_BATTING_SO. Pairs of variables that demonstrate correlation greater than 0.6 are {TEAM_BATTING_3B, TEAM_BATTING_HR}, {TEAM_BATTING_3B, TEAM_BATTING_SO}, {TEAM_BATTING_HR, TEAM_BATTING_SO}.

#Examine `BASERUN` and `FIELDING` variables:

pairs.panels(raw[,c(2,10:13)],
             method = 'pearson')

#All these variables are very roughly linear, and there are no correlations greater than 0.6.

#Examine `PITCHING` variables:

pairs.panels(raw[,c(2,14:17)],
             method = 'pearson')

#These need to be examined again after transformation.

#We're also interested in uncovering any correlations between variables, since these could impact the performance of a linear model. 

tmp <- raw %>%
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

#Group all variables with theoretical positive effect first, then theoretical negative effect. We would expect positive correlations among variables with positive effects, positive correlations among variables with negative effects, and negative correlations among variables with opposite effects. 
tmp <- tmp[,c(2:7,11,9,17,15,8,10,16,14,12,13)]
correlation <- cor(tmp)
corrplot.mixed(correlation, tl.col = 'black', tl.pos = 'lt')
#But this is not what we see. Instead we see that no variable along, other than some of the batting variables, has correlation greater than 0.2 in either direction with the target. And we also see that some pairs of variables have correlations that are so strong or misdirected that we have reason to doubt the integrity of the data. Note that `TEAM_PITCHING_HR` and `TEAM_BATTING_HR` contain essentially the same data. [And `TEAM_BATTING_SO` and `TEAM_PITCHING_SO` are Jack's basis for grouping.]

#Another unexpected and concerning finding in our exploration is that the variables `TEAM_BATTING_SO` and `TEAM_PITCHING_SO` segment into groups, three of which of which exhibits a very high correlation.

raw %>%
  mutate(SO_factor = case_when(TEAM_BATTING_SO >= TEAM_PITCHING_SO*.96+10~ 'high',
                              (TEAM_BATTING_SO<TEAM_PITCHING_SO*.96+10 & TEAM_BATTING_SO>TEAM_PITCHING_SO*.96-50) ~'med_high',
                              (TEAM_BATTING_SO<TEAM_PITCHING_SO*.96-50 & TEAM_BATTING_SO>TEAM_PITCHING_SO*.96-120) ~'med_low',
                              TEAM_BATTING_SO<TEAM_PITCHING_SO*.96-120 ~'low')) %>%
  filter(TEAM_PITCHING_SO < 2000) %>%
  ggplot(aes(x = TEAM_PITCHING_SO, y = TEAM_BATTING_SO, colour = SO_factor)) +
  geom_point()

#There is no theoretical reason to expect such high correlations between the number of strikeouts a team incurs while batting, and the number of strikeouts a team achieves while pitching. However, it may be useful to divide the data into the four groups suggested by these relationships for purposes of analysis.

#Our exploration of the data suggests the following transformations:

#CHANGE NAMES
names(raw) <- gsub('TEAM_', '', x = names(raw))

#CAP VARIABLES
raw$FIELDING_E[raw$FIELDING_E > 1000] <- 1000
raw$PITCHING_BB[raw$PITCHING_BB > 1000] <- 1000
raw$PITCHING_H[raw$PITCHING_H > 3250] <- 3250
raw$PITCHING_SO[raw$PITCHING_SO > 2000] <- 2000

#Impute missing values using column medians:

raw <- mutate_all(raw, ~ifelse(is.na(.), median(., na.rm = TRUE), .))

#Add variables based on groupings of `TEAM_BATTING_SO` AND `TEAM_PITCHING_SO`:

###
raw <- raw %>% mutate(SO_factor = case_when(BATTING_SO >= PITCHING_SO*.96+10 ~ 'high',
                                           (BATTING_SO<PITCHING_SO*.96+10 & BATTING_SO>PITCHING_SO*.96-50) ~'med_high',
                                           (BATTING_SO<PITCHING_SO*.96-50 & BATTING_SO>PITCHING_SO*.96-120) ~'med_low',
                                           BATTING_SO<PITCHING_SO*.96-120 ~'low'))
###

#Move response to the last col, drop INDEX, drop TEAM_BATTING_HBP:
raw <- raw[,c(3:10,12:18,2)]

#Construct new variables to reduce the number of pairs of correlated variables, and to better account for the structure of a baseball season. Note this includes a dummy variable to divide the dataset into "eras", one where BATTING_SO was typically less than 750, and one where it was typically greater:

raw <- raw %>%
  mutate("BASERUN_NET_SB" = BASERUN_SB - BASERUN_CS) %>%
  mutate("OFFENSE_OBP" = (BATTING_H + BATTING_BB)/(BATTING_H + BATTING_BB - BASERUN_CS + (162*27))) %>%
  mutate("DEFENSE_OBP" = (PITCHING_H + FIELDING_E + PITCHING_BB - FIELDING_DP)/(PITCHING_H + FIELDING_E + PITCHING_BB - FIELDING_DP + (162*27))) %>%
  mutate("TOT_AT_BATS" = BATTING_H + BATTING_BB - BASERUN_CS + (162*27))

#Drop original variables used in constructions above:

raw <- raw %>%
  select(-c(BASERUN_SB,
            BASERUN_CS,
            BATTING_H,
            BATTING_BB,
            PITCHING_H,
            FIELDING_E,
            PITCHING_BB,
            FIELDING_DP)
  )

#Move response to end again:

raw <- raw[,c(1:7,9:12,8)]

############################
ggplot(data = raw, aes(x = PITCHING_SO, y = BATTING_SO, colour = SO_factor)) + 
  geom_point()
############################

# Section 3. Modeling.

#ggpairs(data = raw, mapping = ggplot2::aes(colour=SO_factor, alpha = 0.2))
#See desktop for this plot.

#Still have to drop one of {BAT_HR, PITCH_HR}, {BAT_SO, PITCH_SO}, {OFFENSE_OBP, TOT_AT_BAT}

pruned <- raw %>%
  select(-c(BATTING_HR,
            BATTING_SO,
            TOT_AT_BATS))

#All remaining variables:

lm1 <- lm(TARGET_WINS ~ ., data = pruned)
summary(lm1)

plot(lm1)
#The residuals plot seems messed up. I don't know how to interpret the residuals vs. leverage plot. Let's do a total model on each group individually:

low <- pruned %>%
  filter(SO_factor == 'low') %>%
  select(-SO_factor)

med_low <- pruned %>%
  filter(SO_factor == "med_low") %>%
  select(-SO_factor)

med_high <- pruned %>%
  filter(SO_factor == "med_high") %>%
  select(-SO_factor)

high <- pruned %>%
  filter(SO_factor == "high") %>%
  select(-SO_factor)

lm_low <- lm(TARGET_WINS ~ ., data = low)
lm_med_low <- lm(TARGET_WINS ~ ., data = med_low)
lm_med_high <- lm(TARGET_WINS ~ ., data = med_high)
lm_high <- lm(TARGET_WINS ~ ., data = high)

summary(lm_low)
summary(lm_med_low)
summary(lm_med_high)
summary(lm_high)

#Considering SO_factor as 1/0:
tmp <- pruned %>%
  mutate("two_lev" = ifelse(SO_factor %in% c("low", "med_low"), 1, 0)) %>%
  select(-SO_factor)
tmp_lm <- lm(TARGET_WINS ~ ., data = tmp)
summary(tmp_lm)

