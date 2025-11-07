#-----------------------------------------------------------
# MX500 Assignment 1 Problem solving task 2
#-----------------------------------------------------------
# Student details:
# Last name: Hand
# First name: Melanie
# Student number: n11484594


# Load required libraries
library(tidyverse)
library(ggplot2)
library(GGally)
library(broom)

#-----------------------------------------------------------
# Data Structure
#-----------------------------------------------------------

# Read in PST2 Data
CPU_data <- read_csv("PST2_randomised_TP3_2024.csv")

# Check dimensions
dim(CPU_data)

# Rename columns to remove spaces (which can cause issues)
colnames(CPU_data) <- c("Year", "Transistors", "ClockMHz", "PowerDensity", "Cores", "SIS_ID")

# Filter the data to remove any rows with my student number but keep all rows with NA in this column
CPU_data <- filter(CPU_data, SIS_ID != "n11484594" | is.na(SIS_ID))

# Now remove the SIS_ID column
CPU_data <- subset(CPU_data, select = -c(SIS_ID))

# Display dimensions of the dataframe
dim(CPU_data)

# Transform the data to include a column TimeSince which is the time in years since 1970
CPU_data <- mutate(CPU_data, TimeSince = Year-1970)

# Determine minimum, median and maximum values
summary(CPU_data, na.rm=T)
CPU_data_summary <- summarise_all(CPU_data, list(min, median, max), na.rm=T)
CPU_data_summary

#-----------------------------------------------------------
# Graphical Summaries
#-----------------------------------------------------------

# Create a pairwise plot showing the relationship between each variable in the dataset
# Exclude the Years column
ggpairs(CPU_data, columns=2:6,
        lower=list(continuous="smooth"),
        diag=list(continuous="densityDiag")) +
  theme_bw()

# Create a plot to show variability in the ClockMHz and TimeSince variables
# Will use a scatterplot

# First create a new column which is the log of ClockMHz
CPU_data <- mutate(CPU_data, log.ClockMHz = log(ClockMHz))

# Also create a new column of log of TimeSince to check if this helps linearise the data
CPU_data <- mutate(CPU_data, log.TimeSince = log(TimeSince))

# Plot as a scatterplot with a straight line of best fit
ggplot(data=CPU_data, aes(x=TimeSince, y=log.ClockMHz, )) +
  geom_point() +
  geom_smooth(method="lm", colour="Black",se=FALSE) +
  theme_bw() +
  labs(y="Natural log of Clock Speed (MHz)", x="Years since 1970", 
       title="Relationship between clock speed in affordable CPUs and years since 1970") +
  theme(plot.title=element_text(hjust=0.5))

#-----------------------------------------------------------
# Linear Regression
#-----------------------------------------------------------

## Linear Model

# Fit a linear model to the variables clock speed (log.ClockMHz) and time since 1970 (TimeSince)
# Clock speed is the dependent variable (Y), time since 1970 is the independent variable (X)
clock_lm <- lm(data=CPU_data, log.ClockMHz ~ TimeSince)
summary(clock_lm)

# Obtain parameter estimates, confidence intervals and p values for each of the parameters in the model. 
tidy(clock_lm, conf.int = T, conf.level = 0.95) %>%
  select(term, estimate, conf.low, conf.high, p.value)

# Determine amount of variability explained by the model (R^2 value)
glance(clock_lm)

## Analysis of residuals

# Create a plot that shows how the residuals vary with the values fitted through the regression model.
# First use the fortify function to get this fitted and residual values
clock_lm_fort <- fortify(clock_lm)

# Now create a scatter plot of fitted values vs residuals
ggplot(data=clock_lm_fort, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = expression(paste("Fitted (",hat(y[i]), ")")), 
       y=expression(paste("Residual (",epsilon[i],")")),
       title=expression(paste("Residual homogeneity check: ",hat(y[i]), " vs ",epsilon[i],""))) +
  theme(plot.title=element_text(hjust=0.5))

# Create a scatter plot of residuals and Power Density
ggplot(data=clock_lm_fort, aes(x=PowerDensity, y=.resid)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Power Density",
       y=expression(paste("Residual (",epsilon[i],")")),
       title=expression(paste("Power Density vs Residuals",epsilon[i],""))) +
  theme(plot.title=element_text(hjust=0.5))

# Create a QQ plot that compares the standardised residuals to a standard normal distribution.
ggplot(data=clock_lm_fort, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline(ntercept=0, slope=1) +
  coord_equal()+
  xlab("Theoretical (Z ~ N(0,1))") +
  ylab("Sample") + coord_equal() + theme_bw() +
  labs(title = "Quantile-Quantile plot comparing standardised residuals to a standard normal distribution") +
  theme(plot.title=element_text(hjust=0.5))

#-----------------------------------------------------------
# Advanced Regression
#-----------------------------------------------------------

## Multiple Explanatory Variables

# Now fit a linear model to predict clock speed using two explanatory variables: TimeSince and PowerDensity
clock_lm_2 <- lm(data=CPU_data, log.ClockMHz ~ TimeSince + PowerDensity)

# Produce a table that includes the parameter name, estimate and 95% confidence interval
tidy(clock_lm_2, conf.int = TRUE, conf.level = 0.95)

# Determine R^2 value to see how much variability is explained by the model
glance(clock_lm_2)

## Residual Analysis

# Create a plot that shows how the residuals vary with the values fitted through the multivariate model.
# First use the fortify function to get this fitted and residual values
clock_lm_2_fort <- fortify(clock_lm_2)

# Now create a scatter plot of fitted values vs residuals
ggplot(data=clock_lm_2_fort, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = expression(paste("Fitted (",hat(y[i]), ")")), 
       y=expression(paste("Residual (",epsilon[i],")")),
       title=expression(paste("Multivariate model residual homogeneity check: ",hat(y[i]), " vs ",epsilon[i],""))) +
  theme(plot.title=element_text(hjust=0.5))

# Create a QQ plot that compares the standardised residuals to a standard normal distribution.
ggplot(data=clock_lm_2_fort, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline(ntercept=0, slope=1) +
  coord_equal()+
  xlab("Theoretical (Z ~ N(0,1))") +
  ylab("Sample") + coord_equal() + theme_bw() +
  labs(title = "Quantile-Quantile plot comparing standardised residuals to a \n standard normal distribution from the multivariate model",width=50) +
  theme(plot.title=element_text(hjust=0.5))

## Model Choice

# Perform an F-test to determine the best model (the model that explains the most variability)

# H0: Amount of variation explained by the reduced model is equal to that explained by the full model (null hypothesis)
# H1: Amount of variation explained is greater in the full model (alternative hypothesis)

anova(clock_lm, clock_lm_2)

# The p-value = 1.1983e-13 therefore there is sufficient evidence to reject the 
# null hypothesis. As such, the multivariate model explains a greater amount of 
# uncertainty and can be considered the better model.

## Best Model

# Will try to create another model to see if it can be improved

# Model A
# As the residuals vs fitted were still curved with the multivariate model,
# will first try a polynomial model with 1 variable: TimeSince

clock_lm_poly <- lm(data=CPU_data, log.ClockMHz ~ poly(TimeSince, 2, raw=T))
glance(clock_lm_poly)

# Compare to the multivariate model
anova(clock_lm_2, clock_lm_poly)

# R^2 value is 0.917 so it explains slightly more variability than the multivariate model (0.913)
# But the anova shows that it is not a better model (p = 1.5)

# Model B:
# Will now try a model with 3 explanatory variables:
# Time, number of transistors and power density
# I chose these variables because they are correlated with clock speed

clock_lm_3 <- lm(data=CPU_data, log.ClockMHz ~ TimeSince + Transistors + PowerDensity)
glance(clock_lm_3)

# Compare to the original multivariate model
anova(clock_lm_2, clock_lm_3)

# R^2 value is 0.926 so it explains more variability than the multivariate model (0.913)
# And the anova shows that it is a better model (p<0.05)

# Model C:
# From the pairwise plot, it appears that the relationship between number of transistors
# and clock speed may be exponential
# The pairplot suggested the following relationships:
# Clock speed is positively correlated with Power Density
# Log of clock speed is positively correlated with time
# Log of clock speed is positively correlated with number of transistors
# Number of transistors is positively correlated with number of cores
# Will therefore try first transforming the transistors variable prior to modelling

CPU_data <- mutate(CPU_data, log.Transistors = log(Transistors))

# And do the same for power density and cores
CPU_data <- mutate(CPU_data, log.PowerDensity = log(PowerDensity))
CPU_data <- mutate(CPU_data, log.Cores = log(Cores))

# Now try to repeat model B but use the log of all variables except for time

clock_lm_4 <- lm(data=CPU_data, log.ClockMHz ~ TimeSince + log.Transistors + log.PowerDensity)
glance(clock_lm_4)

# Compare to the previous model
anova(clock_lm_3, clock_lm_4)

# R^2 value is 0.947 which is the best so far. But anova says it doesn't explain more variation (p>0.05)

# Model D:
# Will add in cores to model C
clock_lm_5 <- lm(data=CPU_data, log.ClockMHz ~ TimeSince + log.Transistors + log.PowerDensity + log.Cores)
summary(clock_lm_5) # View the model parameters
glance(clock_lm_5) # View the R^2 value

# Compare to the previous model
anova(clock_lm_3, clock_lm_5)

# R^2 value is 0.979 which is the best so far. And anova suggests it explains more variation (p<0.05)

# Compare to the original multivariate model
anova(clock_lm_2, clock_lm_5)

# This comparison also shows that Model D performs better than the original multivariate model

## Residual analysis

# Now check the residuals for Model D
# First use the fortify function to get this fitted and residual values
clock_lm_5_fort <- fortify(clock_lm_5)

# Now create a scatter plot of fitted values vs residuals
ggplot(data=clock_lm_5_fort, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = expression(paste("Fitted (",hat(y[i]), ")")), 
       y=expression(paste("Residual (",epsilon[i],")")),
       title=expression(paste("Residual homogeneity check: ",hat(y[i]), " vs ",epsilon[i],""))) +
  theme(plot.title=element_text(hjust=0.5))

# Create a QQ plot that compares the standardised residuals to a standard normal distribution.
ggplot(data=clock_lm_5_fort, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline(ntercept=0, slope=1) +
  coord_equal()+
  xlab("Theoretical (Z ~ N(0,1))") +
  ylab("Sample") + coord_equal() + theme_bw() +
  labs(title = "Quantile-Quantile plot comparing standardised residuals to a \n standard normal distribution from the multivariate model",width=50) +
  theme(plot.title=element_text(hjust=0.5))

# We can also perform an Anderson-Darling normality test:
library(goftest)
ad.test(clock_lm_5_fort$.stdresid, null = "pnorm")

# Model E:
# The residuals of Model D are still curved, so will try a polynomial again
# This time will try polynomial just using the transistors variable
clock_lm_6 <- lm(data=CPU_data, log.ClockMHz ~ TimeSince + poly(Transistors, 2, raw=T))
glance(clock_lm_6)

# R^2 is 0.921 so this model explains less variation than model D

# Model F:
# Will try removing time from Model D as it had a very small effect on the model
clock_lm_7 <- lm(data=CPU_data, log.ClockMHz ~ log.Transistors + log.PowerDensity + log.Cores)
summary(clock_lm_7) # View the model parameters
glance(clock_lm_7) # View the R^2 value

# Compare to the previous model
anova(clock_lm_7, clock_lm_5)

# R^2 value is 0.979 which is the same as Model D.
# But the anova suggests removing time variable doesn't really improve the model