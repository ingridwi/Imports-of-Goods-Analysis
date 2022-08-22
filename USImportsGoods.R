### Stat 170 Homework 2
### Group Members: Shiqi Liang, Ingrid Wijaya, Jessica Wong
### February 15, 2022

# Section 1 and 2

# (a) and (b)

### Database name (for all three variables):
###   Federal Reserve Economic Data (FRED)
### Variable names:
###   U.S. Imports of Goods by Customs Basis from China (IMPCH)
###   Average Hourly Earnings of Production and Nonsupervisory Employees,
###       Total Private (CEU0500000008)
###   Industrial Production: Non-Durable Nonenergy Consumer Goods (IPB51210N)

### Loading data from FRED using Quandl
### Since we are only considering non-COVID years, we have decided to only look
### at data before Jan 2020.
library(Quandl)
cimport = Quandl(code = "FRED/IMPCH",
                 type = "ts",
                 collapse = "monthly",
                 order = "asc",
                 end_date = "2019-12-31",
                 meta = TRUE)
avg_hr_sal = Quandl(code = "FRED/CEU0500000008",
                    type = "ts",
                    collapse = "monthly",
                    order = "asc",
                    end_date = "2019-12-31",
                    meta = TRUE)
ind_prod =  Quandl(code = "FRED/IPB51210N",
                   type = "ts",
                   collapse = "monthly",
                   order = "asc",
                   end_date = "2019-12-31",
                   meta = TRUE)

### Creating a table with relevant information
description1 = paste('United States imports of goods by customs basis',
                     'from China by month')
description2 = paste('Average hourly earnings of production and',
                     'nonsupervisory employees in the private sector',
                     'of the United States by month')
description3 = paste('U.S. industrial production of non-durable',
                     'nonenergy consumer goods by month')
info = data.frame(variable_short_name = c('FRED/IMPCH',
                                          'FRED/CEU0500000008',
                                          'FRED/IPB51210N'),
                  description = c(description1, description2, description3),
                  training_period = c('1992-1 to 2005-12',
                                      '1992-1 to 2005-12',
                                      '1992-1 to 2005-12'),
                  testing_period = c('2006-1 to 2006-12',
                                     '2006-1 to 2006-12',
                                     '2006-1 to 2006-12'))
info

# (c) and training/testing split from (b)

### Using intersect() to select observations from shared date range
full = ts.intersect(cimport, avg_hr_sal, ind_prod)

### Creating training and testing data sets
### We used years after 1991 in order to avoid the influence of the early
### 1990s recession
### We used years before 2007 in order to avoid the influence of the financial
### crisis of 2007-2008 and the Great Recession
train = window(full, start = c(1992, 1), end = c(2005, 12))
test = window(full, start = c(2006, 1), end = c(2006, 12))

### Because of section 4, we now have 168 observations, but have confirmed that
### in this case at least 150 observations would be acceptable
nrow(train)

# (d)

### Time plot of variable: U.S. imports of goods by customs basis from China
y = train[, 'cimport']
ts.plot(y,
        main = 'U.S. imports of goods by customs basis from China',
        ylab = 'Millions of dollars')

### Seasonal box plot of variable: U.S. imports of goods by customs basis
### from China
boxplot(y ~ cycle(y),
        main = paste('Seasonal boxplot of U.S. imports of goods',
                     '\nby customs basis from China'),
        xlab = 'Month', ylab = 'Millions of dollars')

### Time plot of variable: Average hourly earnings of U.S. private sector
### production and nonsupervisory employees
y = train[, 'avg_hr_sal']
ts.plot(y,
        main = paste('Average hourly earnings of U.S. private sector',
                     '\nproduction and nonsupervisory employees'),
        ylab = 'Dollars per hour')

### Seasonal box plot of variable: Average hourly earnings of U.S. private
### sector production and nonsupervisory employees
boxplot(y ~ cycle(y),
        main = paste('Seasonal box plot of average hourly earnings of U.S.',
                     '\nprivate sector production and nonsupervisory employees'),
        xlab = 'Month', ylab = 'Dollars per hour')

### Time plot of variable: U.S. industrial production of non-durable
### consumer goods
y = train[, 'ind_prod']
ts.plot(y,
        main = 'U.S. industrial production of non-durable consumer goods',
        ylab = 'Index with 2017 as 100')

### Seasonal box plot of variable: U.S. industrial production of non-durable
### consumer goods
boxplot(y ~ cycle(y),
        main = paste('Seasonal box plot of U.S. industrial production',
                     '\nof non-durable consumer goods'),
        xlab = 'Month', ylab = 'Index with 2017 as 100')

# Section 2.1

# (e)

### Dependent variable:
###   U.S. imports of goods by customs basis from China
### Independent variables:
###   Average hourly earnings of U.S. private sector production and
###       nonsupervisory employees
###   U.S. industrial production of non-durable consumer goods

### Performing additive decomposition on the dependent variable
import_add_decomp = decompose(train[, 'cimport'], type = 'add')

### Plotting the additive decomposition as a basis for comparison
plot(import_add_decomp)

### Performing multiplicative decomposition on the dependent variable
import_decomp = decompose(train[, 'cimport'], type = 'mult')

### Plotting the multiplicative decomposition in one view
plot(import_decomp)

### Plotting the trend, seasonal, and random components
### from multiplicative decomposition individually
import_trend = import_decomp$trend
import_season = import_decomp$seasonal
import_ran = import_decomp$random
plot(import_trend,
     main = paste('Trend component of U.S. imports of goods',
                  '\nby customs basis from China'),
     ylab = 'Millions of dollars')
plot(import_season,
     main = paste('Seasonal component of U.S. imports of goods',
                  '\nby customs basis from China'),
     ylab = 'Millions of dollars')
plot(import_ran,
     main = paste('Random component of U.S. imports of goods',
                  '\nby customs basis from China'),
     ylab = 'Millions of dollars')

# (f)

### Finding the ACF of the random component
new_import_ran = window(import_ran, start = c(1992, 7), end = c(2005, 6))
acf(new_import_ran, lag = 50,
    main = paste('ACF of random component of U.S. imports of goods',
                 '\nby customs basis from China'))

### Finding the PACF of the random component
pacf(new_import_ran, lag = 50,
     main = paste('PACF of random component of U.S. imports of goods',
                  '\nby customs basis from China'))

# Section 3

# (g) and Section 3.1

### Time plot of pre-transformed time series: U.S. imports of goods by
### customs basis from China
cimport_train = train[, 'cimport']
ts.plot(cimport_train,
        main = 'U.S. imports of goods by customs basis from China',
        ylab = 'Millions of dollars')

### Trying 2 types of transformations: log, square root
log_cimport_train = ts(log(cimport_train), start = c(1992, 1), freq = 12)
sqrt_cimport_train = ts(sqrt(cimport_train), start = c(1992, 1), freq = 12)
tm = cbind(raw_data = cimport_train,
           log_transfm = log_cimport_train,
           sqrt_transfm = sqrt_cimport_train)

### Comparing time plots of U.S. imports of goods by customs basis
### from China: raw data, after log transformation, and after square
### root transformation
plot.ts(tm, main = 'Pre-transformations: raw data (top), log transform, sqrt transform')

### Comparing time plots of U.S. imports of goods by customs basis
### from China, before and after log transformation
plot(cbind(raw_data = cimport_train, log_transfm = log_cimport_train),
     main = paste('U.S. imports of goods by customs basis from China,',
                  '\nraw data (top) and log transform'))

### Comparing time plots of U.S. imports of goods by customs basis
### from China, before and after square root transformation
plot(cbind(raw_data = cimport_train, sqrt_transfm = sqrt_cimport_train),
     main = paste('U.S. imports of goods by customs basis from China,',
                  '\nraw data (top) and sqrt transform'))

### Proceeding with the log transformation as y.star
y.star = log_cimport_train

### Time plot of time series after pre-differencing transformation:
### Log transformation of U.S. imports of goods by customs basis from China
ts.plot(y.star,
        main = paste('Log transformation of U.S. imports of goods',
                     '\nby customs basis from China'),
        ylab = 'Log of imports of goods')

# Section 3.2

### Checking how much differencing should be done

### Checking first-order regular differencing only, to account for trend
reg_diff = diff(y.star, lag = 1)
ts.plot(reg_diff,
        main = paste('First regular differencing of log transformed U.S.',
                     '\nimports of goods by customs basis from China'),
        ylab = 'first diff of log imports')

### ACF of first-order regular differencing only
acf(reg_diff, lag = 50,
    main = paste('ACF of first regular differencing of log transformed U.S.',
                 '\nimports of goods by customs basis from China'))

### Checking seasonal differencing only, to account for seasonality
seas_diff = diff(y.star, lag = 12)
ts.plot(seas_diff,
        main = paste('Seasonal differencing of log transformed U.S. imports',
                     '\nof goods by customs basis from China'),
        ylab = 'seasonal diff of log imports')

### ACF of seasonal differencing only
acf(seas_diff, lag = 50,
    main = paste('ACF of seasonal differencing of log transformed U.S. imports',
                 '\nof goods by customs basis from China'))

### Checking seasonal differencing of first-order regular differencing,
### to account for both trend and seasonality
seas_reg_diff = diff(reg_diff, lag = 12)
ts.plot(seas_reg_diff,
        main = paste('Seasonally-differenced first-differenced log transform',
                     '\nof U.S. imports of goods by customs basis from China'),
        ylab = 'seas diff of first diff of log imports')

### ACF of seasonal differencing of first-order regular differencing
acf(seas_reg_diff, lag = 50,
    main = paste('ACF of seasonally-differenced first-differenced log transform',
                 '\n of U.S. imports of goods by customs basis from China'))

### Viewing the three ACF plots together for easy comparison, to identify
### which time series is more stationary
par(mfrow = c(3, 1))
acf(reg_diff, lag = 50,
    main = paste('ACF of first regular differencing of log transformed U.S.',
                 '\nimports of goods by customs basis from China'))
acf(seas_diff, lag = 50,
    main = paste('ACF of seasonal differencing of log transformed U.S. imports',
                 '\nof goods by customs basis from China'))
acf(seas_reg_diff, lag = 50,
    main = paste('ACF of seasonally-differenced first-differenced log transform',
                 '\n of U.S. imports of goods by customs basis from China'))

### Decided to continue with the seasonal differencing of first-order
### regular differencing
dev.off()

### Identifying a preliminary model by looking at ACF and PACF together
par(mfrow = c(2, 1))

##### ACF of seasonal differencing of first-order regular differencing
acf(seas_reg_diff, lag = 50,
    main = paste('ACF of seasonally-differenced first-differenced log transform',
                 '\n of U.S. imports of goods by customs basis from China'))

##### PACF of seasonal differencing of first-order regular differencing
pacf(seas_reg_diff, lag = 50,
     main = paste('PACF of seasonally-differenced first-differenced log transform',
                  '\n of U.S. imports of goods by customs basis from China'))

### Decided on MA(1)
dev.off()

# Section 3.3

### Identifying a model for ARIMA notation by looking at ACF and PACF together
par(mfrow = c(2, 1))

### ACF of seasonal differencing of first-order regular differencing
acf(seas_reg_diff, lag = 50, main = 'ACF of (1-B^12)(1-B)X')

### PACF of seasonal differencing of first-order regular differencing
pacf(seas_reg_diff, lag = 50, main = 'PACF of (1-B^12)(1-B)X')

### Decided on ARIMA(2,1,0)(0,1,1)_12
dev.off()

# Section 3.4

### Fitting model: Model 1
model1 = arima(y.star, order = c(2, 1, 0), seas = list(order = c(0, 1, 1), 12))
model1

### Check the residuals for model 1
### ACF of residuals
acf(resid(model1), lag = 50, main = "ACF of residuals of model 1")
### Histogram of residuals
hist(resid(model1), main = "Histogram of residuals of model 1")

### Do the Ljung-Box test (white noise test) for model 1 
Box.test(model1$residuals, lag = 12, type = 'Ljung')
Box.test(model1$residuals, lag = 24, type = 'Ljung')
Box.test(model1$residuals, lag = 50, type = 'Ljung')

### To see if we are missing something, check ACF and PACF together
par(mfrow = c(2, 1))
acf(seas_reg_diff, lag = 50, main = 'ACF of (1-B^12)(1-B)X')
pacf(seas_reg_diff, lag = 50, main = 'PACF of (1-B^12)(1-B)X')
dev.off()

### Fitting model: Model 2
### Regular part: No changes made
### Seasonal part: Tried MA(2) instead of MA(1)
model2 = arima(y.star, order = c(2, 1, 0), seas = list(order = c(0, 1, 2), 12))
model2

### Check the residuals for model 2
### ACF of residuals 
acf(resid(model2), lag = 50, main = "ACF of residuals of model 2")
### Histogram of residuals 
hist(resid(model2), main = "Histogram of residuals of model 2")

### Do the Ljung-Box test (white noise test) for model 2
Box.test(model2$residuals, lag = 12, type = 'Ljung')
Box.test(model2$residuals, lag = 24, type = 'Ljung')
Box.test(model2$residuals, lag = 50, type = 'Ljung')

### Model selection

### Comparing RMSE and AIC between model 1 and model 2
df = data.frame(RMSE = c(sqrt(model1$sigma2), sqrt(model2$sigma2)),
                AIC = c(model1$aic, model2$aic))
rownames(df) <- c("Model 1", "Model 2")
df

### Final model - Model 2
final_model = arima(y.star, order = c(2, 1, 0),
                    seas = list(order = c(0, 1, 2), 12))
final_model

### Check if model is stationary for AR in regular part
Mod(polyroot(c(1, 0.5883, 0.4387)))

### Check if model is invertible for MA in seasonal part
Mod(polyroot(c(1, -0.8062, 0.2315)))

# Section 3.5

### Forecasts: Out-of-sample forecasts and forecast errors
### This forecasts the log of imports (not the units that we want)
forecast = predict(final_model, 12)
ts.plot(train[, 'cimport'],
        main = 'U.S. imports of goods by customs basis from China',
        ylab = 'Millions of dollars')

### Convert forecast to original units of imports by undoing the log transform 
forecast.value = ts(exp(forecast$pred), start = c(2006, 1), end = c(2006, 12),
                    freq = 12)

### 95% prediction intervals
### Convert forecast to original units to imports by undoing the log transform
ci.low = ts(exp(forecast$pred - 1.96 * forecast$se), 
            start = c(2006, 1), end = c(2006, 12), freq = 12)
ci.high = ts(exp(forecast$pred + 1.96 * forecast$se),
             start = c(2006, 1), end = c(2006, 12), freq = 12)

### Data frame of observed values, fitted values, confidence intervals, and
### standard errors
imports.test = test[, 'cimport']
df = data.frame("imports" = imports.test, 
                "forecast" = forecast.value,
                "forecast interval" = cbind(ci.low, ci.high),
                "std error" = forecast$se)
df

### Plot forecast, the data and the prediction intervals
imports.full = window(cimport, start = c(1992, 1), end = c(2006, 12))

ts.plot(cbind(imports.full, forecast.value, ci.low, ci.high),
        lty = c(1, 3, 2, 2), col = c("black", "red", "blue", "blue"), 
        main = paste("U.S. imports of goods by customs basis from China:",
                     "\nPredicted monthly values from best model"),
        ylab = "Millions of dollars")
legend("topleft", lty = c(1, 3, 2, 2),
       text.col = c("black", "red", "blue", "blue"), 
       legend = c("imports", "forecast", "lower bound of CI",
                  "upper bound of CI"),
       text.font = 1, cex = 1)
abline(v = 2006)

#### Close Up Plot of only the forecast and the observed data of last 12 months
ts.plot(cbind(imports.test, forecast.value, ci.low, ci.high),
        lty = c(1, 3, 2, 2),
        col = c("black", "red", "blue", "blue"),
        main = paste("U.S. imports of goods by customs basis from China:",
                     "\nForecast and observed data from 2006"),
        ylab = "Millions of dollars")
legend("topleft", lty = c(1, 3, 2, 2), 
       text.col = c("black", "red", "blue", "blue"), 
       legend = c("imports", "forecast", "lower bound of CI",
                  "upper bound of CI"), 
       text.font = 1, cex = 1)

### RMSE of the forecast.
RMSE = sqrt(mean((imports.test - forecast.value)^2))
RMSE

# Section 4

# Section 4.1

### Define variables and inspect transformations of independent variables
y = train[,'cimport']
x1 = train[,'avg_hr_sal']
x2 = train[,'ind_prod']
plot(y)
plot(I(x1^(-1.2)))
plot(I(x2^(1/2)))

### Create dummy variables to simulate seasonality
time=seq(1:length(y))
D1=rep(c(1,0,0,0,0,0,0,0,0,0,0,0),(length(y)/12))
D2=rep(c(0,1,0,0,0,0,0,0,0,0,0,0),(length(y)/12))
D3=rep(c(0,0,1,0,0,0,0,0,0,0,0,0),(length(y)/12))
D4=rep(c(0,0,0,1,0,0,0,0,0,0,0,0),(length(y)/12))
D5=rep(c(0,0,0,0,1,0,0,0,0,0,0,0),(length(y)/12))
D6=rep(c(0,0,0,0,0,1,0,0,0,0,0,0),(length(y)/12))
D7=rep(c(0,0,0,0,0,0,1,0,0,0,0,0),(length(y)/12))
D8=rep(c(0,0,0,0,0,0,0,1,0,0,0,0),(length(y)/12))
D9=rep(c(0,0,0,0,0,0,0,0,1,0,0,0),(length(y)/12))
D10=rep(c(0,0,0,0,0,0,0,0,0,1,0,0),(length(y)/12))
D11=rep(c(0,0,0,0,0,0,0,0,0,0,1,0),(length(y)/12))

### Check if residuals of the multiple regression model are white noise,
### starting by inspecting ACF and histogram
model1 = lm(I(y^(1/4))~I(x1^(-1.2))+I(x2^(1/2))+time+D3+D7+D8+D9+D10+D11+D1)
summary(model1)
acf(model1$residuals, main = 'ACF of multiple regression model residuals',
    lag = 50)
hist(model1$residuals, main = 'Histogram of multiple regression model residuals',
     xlab = 'residual')

### Use Ljung Box test to check if residuals of the multiple regression model
### are white noise
Box.test(model1$residuals, lag = 20, type = 'Ljung')

### Inspect ACF and PACF plots of residuals of multiple regression model
### to identify an ARMA model
plot(model1$residuals, type = 'l',
     main = 'Residuals of multiple regression model',
     ylab = 'residual')
par(mfrow = c(2, 1))
acf(model1$residuals, main = 'ACF of multiple regression model residuals',
    lag = 70)
pacf(model1$residuals, main = 'PACF of multiple regression model residuals',
     lag = 70)
dev.off()

### Fit ARMA model for residuals
resmodel1 = arima(model1$residuals, order = c(3, 0, 0), include.mean = F)

### Check if residuals of the residuals are white noise,
### starting by inspecting plot, ACF, and histogram
plot(resmodel1$residuals, main = 'Residuals of the residuals',
     ylab = 'residual')
acf(resmodel1$residuals, main = 'ACF of the residuals of the residuals',
    lag = 50)
hist(resmodel1$residuals, main = 'Histogram of the residuals of the residuals',
     xlab = 'residual')

### Use Ljung Box test to check if residuals of the residuals are white noise
Box.test(resmodel1$residuals, lag = 50, type = 'Ljung')

### Obtaining coefficients for model of residuals
resmodel1$coef

### Fitting gls model with the above coefficients
library(nlme)
modelgls = gls(I(y^(1/4))~I(x1^(-1.2))+I(x2^(1/2))+time+D3+D7+D8+D9+D10+D11+D1,
               correlation = corARMA(c(0.404273424, -0.008906267, 0.226163624),
                                     p = 3))
modelgls$coef

### Creating dummy variables accordingly
time=seq(length(train[,'cimport']),
         length(train[,'cimport'])+length(test[,'cimport'])-1)
D1=rep(c(1,0,0,0,0,0,0,0,0,0,0,0),(length(test[,'cimport'])/12))
D3=rep(c(0,0,1,0,0,0,0,0,0,0,0,0),(length(test[,'cimport'])/12))
D7=rep(c(0,0,0,0,0,0,1,0,0,0,0,0),(length(test[,'cimport'])/12))
D8=rep(c(0,0,0,0,0,0,0,1,0,0,0,0),(length(test[,'cimport'])/12))
D9=rep(c(0,0,0,0,0,0,0,0,1,0,0,0),(length(test[,'cimport'])/12))
D10=rep(c(0,0,0,0,0,0,0,0,0,1,0,0),(length(test[,'cimport'])/12))
D11=rep(c(0,0,0,0,0,0,0,0,0,0,1,0),(length(test[,'cimport'])/12))

### Forecasting and computing RMSE for new gls model with test data
newdata = data.frame(x1 = test[,'avg_hr_sal'],
                     x2 = test[,'ind_prod'],
                     time,D1,D3,D7,D8,D9,D10,D11)
cm_forecast = predict(modelgls, newdata)^4
cm_forecast = ts(cm_forecast, start = c(2006, 1), end = c(2006, 12),
                 frequency = 12)
test_y = ts(test[,'cimport'], start = c(2006, 1), end = c(2006, 12),
            frequency = 12)
ts.plot(cbind(test_y, cm_forecast), gpars = list(col = c("red","blue")),
        main = "Multiple regression model: forecasted values compared to test data")
legend("topleft", legend = c("real value", "forecasted value"),
       col = c("red", "blue"), lty = 1)
RMSE = sqrt(mean((test_y - cm_forecast)^2))
RMSE

# Section 4.2

### Creating dummy variables
time=seq(1:length(y))
timesq = time^2
timecb = time^3
tr = c(rep(c(0),108),rep(c(1),length(y)-108))
D1=rep(c(1,0,0,0,0,0,0,0,0,0,0,0),(length(y)/12))
D2=rep(c(0,1,0,0,0,0,0,0,0,0,0,0),(length(y)/12))
D3=rep(c(0,0,1,0,0,0,0,0,0,0,0,0),(length(y)/12))
D4=rep(c(0,0,0,1,0,0,0,0,0,0,0,0),(length(y)/12))
D5=rep(c(0,0,0,0,1,0,0,0,0,0,0,0),(length(y)/12))
D6=rep(c(0,0,0,0,0,1,0,0,0,0,0,0),(length(y)/12))
D7=rep(c(0,0,0,0,0,0,1,0,0,0,0,0),(length(y)/12))
D8=rep(c(0,0,0,0,0,0,0,1,0,0,0,0),(length(y)/12))
D9=rep(c(0,0,0,0,0,0,0,0,1,0,0,0),(length(y)/12))
D10=rep(c(0,0,0,0,0,0,0,0,0,1,0,0),(length(y)/12))
D11=rep(c(0,0,0,0,0,0,0,0,0,0,1,0),(length(y)/12))

### Check if residuals of the residuals are white noise,
### using histogram, Box Ljung test, ACF, and PACF
model2=lm(log(y)~time+timesq+timecb+tr+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11)
hist(model2$residuals,
     main = 'Histogram of residuals of dummy variable regression',
     xlab = 'residuals')
Box.test(model2$residuals, lag = 20, type = 'Ljung')
par(mfrow = c(2, 1))
acf(model2$residuals,
    main = 'ACF of residuals of dummy variable regression', lag = 50)
pacf(model2$residuals,
     main = 'PACF of residuals of dummy variable regression', lag = 50)
dev.off()

### Fit ARMA(3,0,1) model to residuals and check if residuals of residuals
### are white noise using plot, ACF, histogram, and Box Ljung test
resmodel2 = arima(model2$residuals, order = c(3, 0, 1), include.mean = F)
plot(resmodel2$residuals, main = 'Residuals of the residuals of dummy variable regression')
acf(resmodel2$residuals,
    main = 'ACF of the residuals of the residuals of dummy variable regression')
hist(resmodel2$residuals,
     main = 'Histogram of the residuals of the residuals of dummy variable regression',
     xlab = 'residual')
Box.test(resmodel2$residuals, lag = 40, type = 'Ljung')

### Obtaining coefficients for gls model
resmodel2$coef

### Fitting gls model
library(nlme)
modelgls = gls(log(y)~time+timesq+timecb+tr+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11,
               correlation = corARMA(c(-0.1768536, 0.1328152, 0.1462675, 0.5403978),
                                     p = 3, q = 1))
modelgls$coefficients

### Creating dummy variables accordingly
time=seq(length(train[,'cimport']),length(train[,'cimport'])+length(test[,'cimport'])-1)
timesq = time^2
timecb = time^3
tr = rep(1,length(test[,'cimport']))
D1=rep(c(1,0,0,0,0,0,0,0,0,0,0,0),(length(test[,'cimport'])/12))
D2=rep(c(0,1,0,0,0,0,0,0,0,0,0,0),(length(test[,'cimport'])/12))
D3=rep(c(0,0,1,0,0,0,0,0,0,0,0,0),(length(test[,'cimport'])/12))
D4=rep(c(0,0,0,1,0,0,0,0,0,0,0,0),(length(test[,'cimport'])/12))
D5=rep(c(0,0,0,0,1,0,0,0,0,0,0,0),(length(test[,'cimport'])/12))
D6=rep(c(0,0,0,0,0,1,0,0,0,0,0,0),(length(test[,'cimport'])/12))
D7=rep(c(0,0,0,0,0,0,1,0,0,0,0,0),(length(test[,'cimport'])/12))
D8=rep(c(0,0,0,0,0,0,0,1,0,0,0,0),(length(test[,'cimport'])/12))
D9=rep(c(0,0,0,0,0,0,0,0,1,0,0,0),(length(test[,'cimport'])/12))
D10=rep(c(0,0,0,0,0,0,0,0,0,1,0,0),(length(test[,'cimport'])/12))
D11=rep(c(0,0,0,0,0,0,0,0,0,0,1,0),(length(test[,'cimport'])/12))

### Forecasting and computing RMSE for new gls model with test data
newdata = data.frame(time,timesq,timecb,tr,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11)
dp_forecast = exp(predict(modelgls, newdata))
dp_forecast = ts(dp_forecast, start = c(2006, 1), end = c(2006, 12),
                 frequency = 12)
test_y = ts(test[,'cimport'], start = c(2006, 1), end = c(2006, 12),
            frequency = 12)
ts.plot(cbind(test_y, dp_forecast), gpars = list(col = c("red", "blue")),
        main = "Dummy variable regression: forecasted values compared to test data")
legend("topleft", legend = c("real value", "forecasted value"),
       col = c("red", "blue"), lty = 1)
RMSE = sqrt(mean((test_y - dp_forecast)^2))
RMSE

# Section 5

### Time plot of variable: U.S. imports of goods by customs basis
### from China
ts.plot(train[, 'cimport'],
        main = 'U.S. imports of goods by customs basis from China',
        ylab = 'Millions of dollars')

### Seasonal box plot of variable: U.S. imports of goods by customs basis
### from China
y = train[, 'cimport']
boxplot(y ~ cycle(y),
        main = paste('Seasonal boxplot of U.S. imports of goods',
                     '\nby customs basis from China'),
        xlab = 'Month', ylab = 'Millions of dollars')

### Using multiplicative Seasonal component exponential smoothing model
### Performing seasonal Holt Winters smoother
imports.train = train[, 'cimport']
imports.hw = HoltWinters(imports.train, seasonal = "mult")  
imports.hw

### Plot of fitted and data
plot(imports.hw, main = paste("Observed and fitted of",
                              "\nmultiplicative seasonal exponential smoothing for imports"),
     ylab = "Millions of dollars")
legend("topleft", legend = c("imports", "fitted"), 
       lty = c(1, 1), col = c("black", "red"))

### Fitted values from Holt-Winters Filtering of imports
fitted.hw = fitted(imports.hw)
head(fitted.hw)

### Plot each component fitted by Holt Winters of imports
plot(fitted.hw, main = paste("Plot of each component fitted from performing",
                             "\nmultiplicative exponential smoothing for imports"))

### Forecast of test data (12 steps ahead) and 95% percent prediction intervals
forecast.hw = predict(imports.hw, n.ahead = 12, prediction.interval = TRUE,
                      level = 0.95)
forecast.hw

### Plot of fitted, data and forecast 
ts.plot(imports.train, forecast.hw[, 1], col = c(1:2),
        main = paste("Multiplicative seasonal exponential",
                     "\nsmoothing for imports, 1992:1-2005:12"),
        lty = 1:2, ylab = "Millions of dollars")
lines(fitted.hw[, 1], lty = 1, col = "red")
lines(forecast.hw[, 2], lty = 2, col = "blue")
lines(forecast.hw[, 3], lty = 2, col = "blue")
legend("topleft", legend = c("imports", "forecast", "fitted",
                             "upper bound of PI", "lower bound of PI"), 
       lty = c(1, 2, 1, 2, 2), col = c("black", "red", "red", "blue", "blue"))

### Up Close Plot of test data and forecast data
ts.plot(cbind(imports.test, predict(imports.hw, n.ahead = 12)),
        col = c("black", "red"), lty = c(1, 2),
        main = "12 step ahead forecast of imports \n2006:1-2006:12", 
        ylab = "Millions of dollars")
lines(forecast.hw[, 2], lty = 2, col = "blue")
lines(forecast.hw[, 3], lty = 2, col = "blue")
legend("topleft", legend = c("imports test 12 months", "forecast", 
                             "upper bound of PI", "lower bound of PI"), 
       lty = c(1, 2, 2, 2), col = c("black", "red", "red", "blue", "blue"))

### Calculate RMSE
RMSE = sqrt(mean((imports.test - forecast.hw[, 1])^2))
RMSE

# Section 6

### Creating table to compare different model forecasts and their RMSEs,
### along with a column for the average forecast
comparison = data.frame("Date" = c("Jan 2006", "Feb 2006", "Mar 2006",
                                   "Apr 2006", "May 2006", "Jun 2006",
                                   "Jul 2006", "Aug 2006", "Sep 2006",
                                   "Oct 2006", "Nov 2006", "Dec 2006"),
                        "Raw Data Values" = as.numeric(imports.test),
                        "ARIMA Modeling Forecast" = as.numeric(forecast.value),
                        "Multiple Regression: Causal Model" =
                          as.numeric(cm_forecast),
                        "Multiple Regression: Dummies and Polynomials Only" =
                          as.numeric(dp_forecast),
                        "Exponential Smoothing" = as.numeric(forecast.hw[, 1]))
comparison$`Average Forecast` <- apply(comparison[, 3:6], 1, mean)
rmse_am <- sqrt(mean((comparison[, 3] - comparison[, 2])^2))
rmse_cm <- sqrt(mean((comparison[, 4] - comparison[, 2])^2))
rmse_dp <- sqrt(mean((comparison[, 5] - comparison[, 2])^2))
rmse_es <- sqrt(mean((comparison[, 6] - comparison[, 2])^2))
rmse_af <- sqrt(mean((comparison[, 7] - comparison[, 2])^2))
rmses <- c(rmse_am, rmse_cm, rmse_dp, rmse_es, rmse_af)
comparison <- rbind(comparison, c("RMSE", NA, rmses))
comparison

#Discussion wk 8

# dependent: imports 
log_cimport_train = ts(log(train[, 'cimport']), start = c(1992, 1), freq = 12)
imports_log_reg = diff(log_cimport_train, lag =1)
imports_log_seas_reg = diff(imports_log_reg, lag = 12)
acf(imports_log_seas_reg, lag=50)

# independent: earnings 
earnings_reg = diff(train[, 'avg_hr_sal'], lag = 1)
acf(earnings_reg, lag=50)

# independent: production 
prod_reg = diff(train[, 'ind_prod'], lag = 1)
prod_seas_reg = diff(prod_reg, lag = 12)
acf(prod_seas_reg, lag=50)

# ccf
imports = imports_log_seas_reg
earnings = earnings_reg
production = prod_seas_reg

## dep n ind
mymts=cbind(imports, earnings) 
class(mymts) 
acf(mymts[-c(1:12),])

mymts=cbind(imports, production) 
class(mymts) 
acf(mymts)

## ind n ind
mymts=cbind(production, earnings) 
class(mymts) 
acf(mymts[-c(1:12),])
