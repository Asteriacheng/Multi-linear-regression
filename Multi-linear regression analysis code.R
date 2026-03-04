rm(list = ls())

setwd('/Users/richardmin/Desktop/University/ST314/ST314 group project instructions and data 2025-26')

library(dplyr)
library(lattice)
library(lme4)
library(sjstats)  
library(performance)


###########################################################################
# I. DATA SET-UP
###########################################################################

wvs7data <- read.csv("WVS7_emanc.csv")

wvs7data[1:8, ]
head(wvs7data)
names(wvs7data)
str(wvs7data)

# Change country variable to factor 
wvs7data$country <- as.factor(wvs7data$country)


###########################################################################
# II. DESCRIPTIVE ANALYSIS 
###########################################################################

# ===================================================
# 1) Univariate statistics for each variable
# ===================================================

# Summary statistics for emaval
summary(wvs7data$emaval)

# Histogram
hist(wvs7data$emaval,
     main = "",
     xlab = "Emancipative values (emaval)")

# Proportions for binary vars
table(wvs7data$female) / nrow(wvs7data)
table(wvs7data$partner) / nrow(wvs7data)
table(wvs7data$rural) / nrow(wvs7data)

# Employment status distribution
table(wvs7data$empstat)
table(wvs7data$empstat) / nrow(wvs7data)

# Cont vars
summary(wvs7data$age)
summary(wvs7data$educ)
hist(wvs7data$age, main = "", xlab = "Age")
hist(wvs7data$educ, main = "", xlab = "Education level")

# Country level
summary(wvs7data$polity)
summary(wvs7data$GDPpercap1)
summary(wvs7data$incrichest10p)

# ===================================================
# 2) Between and Within-Country Comparisons
# ===================================================

# Between country variations
country_means <- aggregate(emaval ~ country,
                           data = wvs7data,
                           mean)

summary(country_means$emaval)

country_means <- country_means[order(country_means$emaval), ]

dotchart(country_means$emaval,
         labels = country_means$country,
         xlab = "Mean emancipative values by country")

# Within country variations: Argentina Example
argentina <- subset(wvs7data, country == levels(wvs7data$country)[1])
hist(argentina$emaval,
     main = paste("Within-country distribution:", levels(wvs7data$country)[1]),
     xlab = "Emancipative values")

# Within country variations: all countries PDF file
wvs7data$country <- as.factor(wvs7data$country)
countries <- levels(wvs7data$country)
pdf("within_country_emaval_histograms_12pp.pdf",
    width = 8.27, height = 11.69) 
op <- par(no.readonly = TRUE)
par(
  mfrow = c(4, 3),   
  mar   = c(2.5, 2.5, 2, 0.5), 
  cex.main = 1.5,
  cex.lab  = 0.7,
  cex.axis = 0.6
)
for (ctry in countries) {
  dat_ctry <- subset(wvs7data, country == ctry)
  hist(dat_ctry$emaval,
       breaks = seq(0, 100, by = 5),
       main = ctry,
       xlab = "emaval",
       ylab = "")
}
par(op)
dev.off()

# ===================================================
# 3) Bivariate associations between emaval and vars
# ===================================================

# age
cor(wvs7data$emaval, wvs7data$age, use = "complete.obs")
wvs7data$age_group <- cut(wvs7data$age,
                          breaks = c(15,25,35,45,55,65,75,90),
                          include.lowest = TRUE)
age_means <- aggregate(emaval ~ age_group, data = wvs7data, mean)
plot(age_means$emaval,
     xaxt = "n",
     xlab = "Age group",
     ylab = "Mean emancipative values")
axis(1, at = 1:length(age_means$age_group), labels = age_means$age_group)

# educ
cor(wvs7data$emaval, wvs7data$educ, use = "complete.obs")
educ_means <- aggregate(emaval ~ educ, data = wvs7data, mean)
plot(educ_means$educ, educ_means$emaval,
     xlab = "Education level",
     ylab = "Mean emancipative values")

# female
boxplot(emaval ~ female, data = wvs7data,
        names = c("Male", "Female"),
        ylab = "Emancipative values")

# rural
boxplot(emaval ~ rural, data = wvs7data,
        names = c("Urban", "Rural"),
        ylab = "Emancipative values")

# partner
boxplot(emaval ~ partner, data = wvs7data,
        names = c("No partner", "Has partner"),
        ylab = "Emancipative values")

# empstat
boxplot(emaval ~ empstat, data = wvs7data,
        xlab = "Employment status",
        ylab = "Emancipative values")

# polity, GDPpercap1, incrichest10
country_means <- aggregate(emaval ~ country, data = wvs7data, mean)
country_info <- aggregate(cbind(polity, GDPpercap1, incrichest10p) ~ country,
                          data = wvs7data,
                          mean)
country_data <- merge(country_means, country_info, by = "country")
plot(emaval ~ polity, data = country_data,
     xlab = "Polity score",
     ylab = "Mean emancipative values")
plot(emaval ~ GDPpercap1, data = country_data,
     xlab = "GDP per capita",
     ylab = "Mean emancipative values")
plot(emaval ~ incrichest10p, data = country_data,
     xlab = "Percentage income share of richest 10%",
     ylab = "Mean emancipative values")


###########################################################################
# III. MODELLING
###########################################################################

# ==========================================================
# RQ1) Examine country differences in individuals’ 
#      emancipative values
# ==========================================================

rq1 <- lmer(emaval ~ 1 + (1 | country), data = wvs7data, REML = FALSE)
summary(rq1)
performance::icc(rq1)

# ==========================================================
# RQ2) How do individuals’ emancipative values depend 
#      on individual characteristics? To what extent 
#      can country differences be explained by 
#      individual characteristics?
# ==========================================================

rq2 <- lmer(emaval ~ female + age + educ + partner + empstat + rural +
             (1 | country), data = wvs7data, REML = FALSE)
summary(rq2)
anova(rq1, rq2)

var_u0 <- as.numeric(VarCorr(rq1)$country)      # country variance in null model
var_u1 <- as.numeric(VarCorr(rq2)$country)      # country variance after adding X's
prop_reduction_country_var <- (var_u0 - var_u1) / var_u0
prop_reduction_country_var
performance::icc(rq1)
performance::icc(rq2)

# ==========================================================
# RQ3) Do the effects of any individual characteristics
#      differ for men and women?
# ==========================================================

rq3_educ <- lmer(emaval ~ female * educ + age + partner + empstat + rural +
                     (1 | country), data = wvs7data, REML = FALSE)
anova(rq2, rq3_educ)
summary(rq3_educ)

rq3_age <- lmer(emaval ~ female * age + educ + partner + empstat + rural +
                    (1 | country), data = wvs7data, REML = FALSE)
anova(rq2, rq3_age)
summary(rq3_age)

rq3_rural <- lmer(emaval ~ female * rural + age + educ + partner + empstat +
                      (1 | country), data = wvs7data, REML = FALSE)
anova(rq2, rq3_rural)
summary(rq3_rural)

rq3_partner <- lmer(emaval ~ female * partner + age + educ + empstat + rural +
                        (1 | country), data = wvs7data, REML = FALSE)
anova(rq2, rq3_partner)
summary(rq3_partner)

rq3_complete <- lmer(emaval ~ female * (age + educ + partner + rural) + empstat +
                 (1 | country), data = wvs7data, REML = FALSE)

anova(rq2, rq3_complete)
summary(rq3_complete)


# ==========================================================
# RQ4) Do any of the individual characteristics have 
#      contextual effects?
# ==========================================================

# Create country means (level-2 versions) for level-1 predictors
wvs7_ctxt <- wvs7data %>%
  group_by(country) %>%
  mutate(
    age_cm     = mean(age, na.rm = TRUE),
    educ_cm    = mean(educ, na.rm = TRUE),
    partner_cm = mean(partner, na.rm = TRUE),
    rural_cm   = mean(rural, na.rm = TRUE),
    female_cm  = mean(female, na.rm = TRUE)
  ) %>%
  ungroup()

# Within-only model (no contextual effects; just individual-level predictors)
rq4_within <- lmer(
  emaval ~ female + age + educ + partner + rural + empstat + (1 | country),
  data = wvs7_ctxt, REML = FALSE
)
summary(rq4_within)

# Full contextual model (adds country means -> contextual effects)
rq4 <- lmer(
  emaval ~ female + age + educ + partner + rural +
    female_cm + age_cm + educ_cm + partner_cm + rural_cm +
    empstat + (1 | country),
  data = wvs7_ctxt, REML = FALSE
)
summary(rq4)

# Joint test: do the country-mean (contextual) terms add explanatory power?
anova(rq4_within, rq4)

# Tests for individual contextual effects (one at a time)

rq4_age <- lmer(
  emaval ~ female + educ + partner + rural + age + age_cm + empstat + (1 | country),
  data = wvs7_ctxt, REML = FALSE
)
anova(rq4_within, rq4_age)
summary(rq4_age)

rq4_educ <- lmer(
  emaval ~ female + age + partner + rural + educ + educ_cm + empstat + (1 | country),
  data = wvs7_ctxt, REML = FALSE
)
anova(rq4_within, rq4_educ)
summary(rq4_educ)

rq4_partner <- lmer(
  emaval ~ female + age + educ + rural + partner + partner_cm + empstat + (1 | country),
  data = wvs7_ctxt, REML = FALSE
)
anova(rq4_within, rq4_partner)
summary(rq4_partner)

rq4_rural <- lmer(
  emaval ~ female + age + educ + partner + rural + rural_cm + empstat + (1 | country),
  data = wvs7_ctxt, REML = FALSE
)
anova(rq4_within, rq4_rural)
summary(rq4_rural)

rq4_female <- lmer(
  emaval ~ female + age + educ + partner + rural + female_cm + empstat + (1 | country),
  data = wvs7_ctxt, REML = FALSE
)
anova(rq4_within, rq4_female)
summary(rq4_female)


# ==========================================================
# RQ5) How do individuals’ emancipative values depend 
#      on country characteristics, after controlling 
#.     for individual characteristics and their 
#.     contextual effects? To what extent can country 
#      differences be explained by country characteristics?
# ==========================================================

rq5 <- lmer(
  emaval ~ 
    female + age + educ + partner + rural +
    female_cm + age_cm + educ_cm + partner_cm + rural_cm +
    polity + GDPpercap1 + incrichest10p +
    empstat +
    (1 | country),
  data = wvs7_ctxt,
  REML = FALSE
)

summary(rq5)

anova(rq4, rq5)

var_rq4 <- as.numeric(VarCorr(rq4)$country)
var_rq5 <- as.numeric(VarCorr(rq5)$country)

prop_reduction_country_var <- (var_rq4 - var_rq5) / var_rq4
prop_reduction_country_var

VarCorr(rq5)
