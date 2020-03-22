

# Basic Stats in R

### 1. Description

1.1 Summary 

```r
# Numerical desciption
summary(Smoking)
mean(Smoking$amtWeekends, na.rm=T)
```



1.2 Graphical description

- **Interval variables:**  Histogram, box plots, and scatter plot

```R
## graphs for interval variables 
### Histogram
hist(Smoking$age)
# code to add normal curve (www.statmethods.net)
x <- Smoking$age 
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue")
### box plots 
boxplot(Smoking$age)
### scatter plot 
plot(Smoking$amtWeekdays, Smoking$amtWeekends, pch = 16, main = "Smoking", xlab = "Weekdays", ylab = "Weekends") abline(lm(Smoking$amtWeekends~Smoking$amtWeekdays), lty=2, col="red")
```



- **Nominal and ordinal variables:** Bar plots, stacked bar plots

```R
# Example 1: Sex 
sexCount <- table(Smoking$sex)
barplot(sexCount, ylim = c(0,1000), main="Sex")

# Example 2: marital status 
table(Smoking$maritalStatus)
# factor function to reorder the categories before graphing
maritalSort<-factor(Smoking$maritalStatus, levels = c("Single", "Married", "Widowed","Divorced","Separated")) 
maritalCount <- table(maritalSort)
barplot(maritalCount, main = "Marital Status", las = 2)

# Example 3: income 
incomeSort<-factor(Smoking$grossIncome, levels = c("Under 2,600","2,600 to 5,200","5,200 to 10,400","10,400 to 15,600","15,600 to 20,800","20,800 to 28,600","28,600 to 36,400","Above 36,400","Refused","Unknown")) 
incomeCount <- table(incomeSort)
barplot(incomeCount, main = "Income", las = 2)

## Stacked barplots
# Example 1: smoke by region 
smoke_regionCount <- table(Smoking$smoke, regionSort)
barplot(smoke_regionCount, main="Smoke by Region", las = 2, ylim = c(0, 500), legend = rownames(smoke_regionCount))

## grouped barplots
barplot(smoke_regionCount, main="Smoke by Region", las = 2, legend = rownames(smoke_regionCount), beside = T)

marathon <- read.table("marathon.csv", header = TRUE, sep = ",", strip.white = TRUE) 
hist(marathon$Time, breaks = 10)
```



- **Mixed of interval and nominal variables**: boxplot, Scatter Plot

```R
# box plot
boxplot(marathon$Time~marathon$Gender)
# scatter plot
plot(marathon$Year, marathon$Time, col=c("red","blue")[marathon$Gender])
# line chart 
## Example: Year(ts), Gender(binomial), Returning hours(interval)
marathon<-marathon[order(marathon$Year),]
# plot set up 
plot(marathon$Year, marathon$Time, type = "n", col=c("red","blue")[marathon$Gender], xlab = "Year", ylab = "Running Time (hours)") 
# add lines and points 
LineF <- subset(marathon, marathon$Gender=="f") LineM <- subset(marathon, marathon$Gender=="m") 
lines(LineF$Year, LineF$Time, type = "b", col = "red", pch = 22) lines(LineM$Year, LineM$Time, type = "b", col = "blue", pch = 21, lty = 2) 
# add legend and title 
title("Marathon Times") 
legend(1990, 3, c("Male", "Female"), cex = .8, col=c("blue","red"), pch = 21:22, lty = 2:1, title = "Gender")
```



### 2. Distributions

2.1 Binomial Distribution

```R
# Binomial distribution: n = 15, p = .25 # P(X = Value)
dbinom(valuesInt, 15, .25)

# Cumulative distribution function 
pbinom(valuesInt, 15, .25)

# X value to obtain at least the given cumulative probability
qbinom(probs, 15, .25)
```

2. 2 Normal Distribution

```r
# Standard normal (z) is the default 
# Distribution function for finding cumulative probability associated with a value 
pnorm(values)

# X value for a given cumulative probability 
qnorm(probs)

# cumulative probability associated with a value 
pnorm(valuesInt, 4, 3)

# X value for a given cumulative probability 
qnorm(probs, 4, 3)

# create random draws from a normal distrbution 
random_normal <- rnorm(10)
```

2.3 T-distribution

```R
# t distribution: 5 df 
pt(values,5)

qt(probs,5)

```



2.4 Chi-squared distribution

```r
# Chi-squared distribution: 1 df 
pchisq(valuesPos,1)

qchisq(probs,1)
```



2.5 F-distribution

```R
#F distribution: 1, 15 df
pf(valuesPos, 1, 15)

qf(probs, 1, 15)
```



### 3. Estimation

3.1 t-test

- one sample t-test

H0 : true mean of the column is equal to 0

```r
# 90% confidence interval for number of years served
t.test(Congress$years, conf.level = .90)
```

- Exact binomial test

H0 : true proportion with stance = "yes" of that column is equal to 63/534

```r
table(Congress$stance)
p_hat <- 63/534

# exact binomial estimation method 
binom.test(63, 534, p_hat, conf.level = .90)

# using a Wilson score interval method that is generally preferred
prop.test(63, 534, conf.level = .90)

```

????

### 4. Testing

4.1 t-test

```r
# two-tailed test of H0: mu(Years) = 10 
t.test(Congress$years, alternative = "two.sided", mu = 10)

# one-tailed test of H0: mu(money_pro) <= 25,000
t.test(Congress$money_pro, alternative = "greater", mu = 25000)

# one-tailed test of H0: mu(money_con) >= 25,000
t.test(Congress$money_con, alternative = "less", mu = 25000)
```

4.2 binomial test

```r
# two-tailed test for H0: (proportion with stance = "yes") = .10
table(Congress$stance)
p_hat <- 63/534
# exact binomial estimation method 
binom.test(63, 534, p = .10, alternative = "two.sided")
# using a Wilson score interval method that is generally preferred
prop.test(63, 534, p = .10, alternative = "two.sided")

```

4.3 Normality Check

```r
# Checking normality of Years distribution
# histogram: years, with normal curve 
h <- hist(Congress$years) 
# code to add normal curve 
x <- Congress$years 
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue")

# probability plot 
qqnorm(x) 
qqline(x, col = "blue")
## if all the points nearly fall on a straight line, then it's normally distributed.

# goodness of fit test of H0: normal 
# but interpret with caution for large data sets 
shapiro.test(x)
```



### 5. Tables ANOVA

5.1 Multinomial: Testing k proportions for a single variable

- Chi-sq

H0: There is no relationships between two categorical variables

**significant p-value** indicates that the variation between these several classes in their travel distances is bigger than we would expected by chance, and there are reliable differences between those 3 classes in the travel distance they generated. (Ex2)

(*Expected values* are assumed to be the values happened completely by chance)

```r
# An employer wants to know which days of the week employees are absent in a five day work week. 
# The day of the week for a random sample of 60 absences was recorded.
# actual number of absences per day M-F 
ObsCounts <- c(15, 12, 9, 9, 15)
# expected number if all days are the same 
ExpCounts <- rep(12, times = 5) 
# expected probabilities 
ExpProb <- ExpCounts/sum(ExpCounts)
## Multinomial test (Chi-squared test for given probabilities)
chisq.test(ObsCounts, p = ExpProb)

# Transit Railroads is interested in the relationship between travel distance and the ticket class purchased.
# create and save contingency table 
TransitTable <- table(Transit)
# Test of independence 
chisq.test(TransitTable)
# cell-by-cell contributions to observed Chi-Square value: ((O-E)^2)/E
(chisq.test(TransitTable)$residuals)^2
# cell-by-cell expected values 
chisq.test(TransitTable)$expected
```

- One-way ANOVA

**Key assumptions:** 

1.if the number of groups is less than 30, all observations should be assumed to be normally distributed. (Normality check)

2. The standard deviation of all observations should be equal in each level of groups. (Levene's test)

H0 : There is no relationship between the types of snow conditions and the mean number of daily visitors.

In R: **aov (numerical variable ~ groups)**

**Significant p-value** indicates that there is relationship between ...

Note: Need to check the **normality of residuals** for all the groups one by one: Residual = Actual - Category mean 

```R
# Are the mean number of daily visitors to a ski resort the same for three types of snow conditions?
# ANOVA 
fit <- aov(NumbVisitors ~ SnowType)
summary(fit)
# print out table of means for different groups
print(model.tables(fit, "means"))
# Visualize with boxplot
boxplot(NumbVisitors ~ SnowType)

# Assumption checks - Normality of residuals 
# Residual = Actual - Category mean 
# Histogram of residuals, with normal curve 
h <- hist(fit$residuals) 
# code to add normal curve 
x <- fit$residuals 
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue")
# Normal probability plot as a check on the normality assumption
qqnorm(fit$residuals)
qqline(fit$residuals, col = "red")
# goodness of fit test of H0: normal 
shapiro.test(fit$residuals)

# to check equal variances 
# H0: all the variances are equal
library(car)
leveneTest(NumbVisitors, SnowType)

### !!! Only used after the ANOVA accutally reject the H0, which means that there is difference between the values for each groups
# to check all the pairwise contrasts 
# H0: no relationship between a pairwise group
TukeyHSD(fit, conf.level = .90)
```



### 6. Regression

**6.1 Modeling**

H0: beta1 to beta n  are all zeroes

R-sq: how much variability in Y can be explained by X based on the model

RMSE: measures how much variability in Y is unexplained by X 



```R
# scatter plot w/ fitted linear regression line 
plot(houses$Size, houses$Price, pch = 16, main = "Houses", xlab = "House Size (100s of square feet)", ylab = "Sale Price ($1000s)") abline(lm(houses$Price ~ houses$Size), lty=2, col="red")

# fit the model (Y~x)
linefit1 <- lm(houses$Price ~ houses$Size)
summary(linefit1)

# there are also functions for seeing specific features, e.g., # to see the coefficients Beta-hats 
coefficients(linefit1)

# to see the coefficient of determination R-squared
summary(linefit1)$r.squared

# correlation (use positive value since beta-hat-1 > 0)
sqrt(summary(linefit1)$r.squared)

# the observed residuals, epsilon-hats 
resids <- residuals(linefit1)

# standard deviation of the residuals = sqrt(MSE)
summary(linefit1)$sigma

## Multiple Regression
# See some relationships by scatter plot
plot(O_Vacancy, Sales, pch = 16, xlab = "Office Vacancy Rate (%)", ylab = "Sales (100s of sheets)")
# fit the model 
linefit4 <- lm(Sales ~ Permits + Mortgage + A_Vacancy + O_Vacancy)
summary(linefit4)

```

**6.2 Inference**

```r
# confidence intervals for Beta-i 
confint(linefit1, level = .90)

# ANOVA data for simple linear regression 
anova(linefit1)
```

**6.3 Assumptions check**

- The mean of residuals is 0

- Homoscedasticity of residuals or equal variance for all observations

If the assumption holds, the residual points in the standardized residual plot on all the predictors should be randomly scattered above and below the line at 0 as we look from left to right.

```r
# standardized residual plot 
linefit1.stres <- rstandard(linefit1) 
plot(houses$Size, linefit1.stres, pch = 16, main = "Standardized Residual Plot", xlab = "House Size (100s of square feet)", ylab = "Standardized Residuals") 
abline(0,0, lty=2, col="red")

# !!! check multiple predictors with the residuals if it is multiple linear regression
```

- Normality of residuals

```r
# Checking normality of standardized residuals 
# histogram: years, with normal curve 
h <- hist(linefit1.stres) 
# code to add normal curve 
x <- linefit1.stres 
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue")

# normal probability plot 
qqnorm(linefit1.stres, main = "Normal Probability Plot", xlab = "Normal Scores", ylab = "Standardized Residuals") 
qqline(linefit1.stres, col = "red")

# Shapiro-Wilk test
shapiro.test(linefit1.stres)
```

**6.4 Prediction**

```r
# fit the model 
linefit4 <- lm(Sales ~ Permits + Mortgage + A_Vacancy + O_Vacancy) 

# create data frame with desired values of {Xi} for the inference
newdata <- data.frame(Permits = 250, Mortgage = 4, A_Vacancy = 3.5, O_Vacancy = 14)

# 90% confidence interval for the mean 
predict(linefit4, newdata, interval="confidence", level = .90)

# 90% prediction interval for Y 
predict(linefit4, newdata, interval="predict", level = .90)
```

**6.5 Multiple Regression**

```r
# Scatterplot Matrix 
pairs(~ Sales + Permits + Mortgage + A_Vacancy + O_Vacancy, main="Simple Scatterplot Matrix")

# collinearity check: correlations among predictors 
cor(drywall[,2:5])

# individual scatter plots against Y (Sales) 
plot(Permits, Sales, pch = 16, xlab = "Number of Permits", ylab = "Sales of Drywall Sheets (100s)") 
abline(lm(Sales ~ Permits), lty=2, col="red")
```



### 7. Regression Extension

7.1 Polynomial Regression

the betas are not interpretable, because they are mathematically correlated with each other. (collinearity)

```r
# fit the quadratic model 
energy$SizeSqd <- Size^2
attach(energy)
linefitQ <- lm(Usage ~ Size + SizeSqd) 
summary(linefitQ)

# standardized residual plot - on fitted values 
linefitQ.stres <- rstandard(linefitQ) plot(linefitQ$fitted.values, linefitQ.stres, pch = 16, main = "Standardized Residual Plot", xlab = "Fitted Energy Usage (Killowatts/Hour per month)", ylab = "Standardized Residuals") 
abline(0,0, lty=2, col="red")

# scatter plot with fitted quadratic model curve 
XvaluesQ <- seq(1000, 3000, 50) 
YpredictedQ <- linefitQ$coefficients[3]*XvaluesQ^2 + linefitQ$coefficients[2]*XvaluesQ + linefitQ$coefficients[1] 
plot(Size, Usage, pch = 16, xlab = "House Size (square feet)", ylab = "Energy Usage (Killowatts/Hour per month)") 
lines(XvaluesQ, YpredictedQ, type = "l", col = "red")

detach(energy)
```

7.2 Categorical Regression

**beta(Type2) interpretation:** The difference between Y and whether it is type 1 or type 2 holding other predictors fixed

Note: We only need to see the lowest p-value to conclude whether there is a relationship between Y and a nominal predictor holding other predictor fixed.

```r
# convert Type to a factor variable TypeF, i.e., nominal 
cars$TypeF<-factor(cars$Type) 
attach(cars)

# scatter plot with Horsepower 
plot(Horsepower, Price, pch = 16, xlab = "Horsepower", ylab = "Price ($)") 
abline(lm(Price ~ Horsepower), lty=2, col="red")

# fit the simple linear regression model 
linefitH <- lm(Price ~ Horsepower) 
summary(linefitH)

# fit the ANOVA 
fitT <- aov(Price ~ TypeF) 
summary(fitT)

print(model.tables(fitT, "means"))

# to check all the pairwise contrasts 
TukeyHSD(fitT, conf.level = .90)

# collinearity check between 2 predictors 
fitM <- aov(Horsepower ~ TypeF) 
summary(fitM)

# fit the second-order multiple regression model with interaction term (TypeF1 as base category) 
linefitHT2 <- lm(Price ~ Horsepower * TypeF) 
summary(linefitHT2)

# fit the first-order model (TypeF=3 as base category) 
# reorder the data frame with TypeF = 3 as reference 
cars <- within(cars, TypeF <- relevel(TypeF, ref = 3)) 
linefitHTalt <- lm(cars$Price ~ cars$Horsepower + cars$TypeF) summary(linefitHTalt)
```

**7.3 Interaction Regression**

The interaction terms is the **interaction effect**:  captures the interaction of two predictors in their relationships with Y variable



```R
# fit the second-order multiple regression model with interaction term (TypeF1 as base category) 
linefitHT2 <- lm(Price ~ Horsepower * TypeF) 
summary(linefitHT2)
```



### Determining the best model

1. simplicity
2. maximize R-sq (RMSE as low as possible)
3. significant predictors
4. residual assumptions ok
5. logical relationships



omit the variable that has the highest p-value every time.

Because you want a model that is not overfitting, so you should not over focus on the R square.