
knitr::opts_chunk$set(echo = TRUE)



# loading student performance data
performance <- read.csv(file = 'Assignment_1.csv', 
                        col.names = c('Id', 'ses', 'engach', 'mathsach'))

# First 6 rows
head(performance)


## Dataset structure
str(performance)


# Step 1: MANOVA Assumptions

require(GGally)

# checking for Linearity

# pairs plot
ggpairs(performance[,c("engach", "mathsach")],) + 
  ggtitle('Checking for Linearity Assumption')

# correlation coefficients
corr <- cor.test(performance$engach, performance$mathsach)
corr$p.value # pvalue
corr$estimate # correlation coefficient



# checking for normality
library(MVN)
library(mvnormtest)
library(nortest)


# Checking for multivariate normality
mvnormtest::mshapiro.test(t(performance[,3:4]))

res <- mvn(performance[,3:4], mvnTest = 'mardia')
res


# checking for univariate normality

# Shapiro Test
shapiro.test(performance$engach)
shapiro.test(performance$mathsach)

# Anderson-Darling Test
ad.test(performance$engach)
ad.test(performance$mathsach)


# Kolmogorov-Smirnov test
lillie.test(performance$engach)
lillie.test(performance$mathsach)


# univariate normality across outcomes
# engach
tapply(performance$engach, performance$ses, shapiro.test)

# mathsach
tapply(performance$mathsach, performance$ses, shapiro.test)



# checking for adequate sample size
table(performance$ses)


# Homogeneity of variance
library(biotools)
# Checking for Homogeneity of covariance using a boxM test
boxM(data = performance[,3:4], grouping = performance$ses)



## Step 2: MANOVA Test 
# MANOVA test
manova_model <- manova(cbind(engach, mathsach) ~ ses, data=performance)



# Pillai Test
summary(manova_model, test = 'Pillai')

# Wilk Test
summary(manova_model, test = 'Wilk')

# Hotelling-Lawley Test
summary(manova_model, test = 'Hotelling-Lawley')

# Roy Test
summary(manova_model, test = 'Roy')


# Step 3: Univariate ANOVA for each dependent variable
# univariate ANOVA of Dependent variables
summary.aov(manova_model)


# Step 4: Pairwise Multiple comparison Test
# pairwise test using the Bonferroni Adjustments

# for engach
pairwise.t.test(performance$engach, performance$ses, 
                p.adjust.method = 'bonferroni')

# for mathsach
pairwise.t.test(performance$mathsach, performance$ses, 
                p.adjust.method = 'bonferroni')

