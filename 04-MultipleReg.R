## ---- fig.align="center"----------------------------------------------------------------
# Some given data
X_1 <- c(1.9,0.8,1.1,0.1,-0.1,4.4,4.6,1.6,5.5,3.4)
X_2 <- c(66, 62, 64, 61, 63, 70, 68, 62, 68, 66)
Y   <- c(0.7,-1.0,-0.2,-1.2,-0.1,3.4,0.0,0.8,3.7,2.0)
dataset <-  cbind.data.frame(X_1,X_2,Y)
## Compute the OLS estimation
my.lm <- lm(Y ~ X_1 + X_2, data = dataset)
## Plot sample regression surface
library("scatterplot3d") # library for 3d plots
plot3d <- scatterplot3d(x = X_1, y = X_2, z = Y,
          angle=33, scale.y=0.8, pch=16,
          color ="red", main ="OLS Regression Surface")
plot3d$plane3d(my.lm, lty.box = "solid", col=gray(.5), 
          draw_polygon=TRUE)


## ---------------------------------------------------------------------------------------
set.seed(123)
n     <- 100               # Sample size
X     <- runif(n, 0, 10)   # Relevant X variable
X_ir  <- runif(n, 5, 20)   # Irrelevant X variable
error <- rt(n, df = 10)*10  # True error
Y     <- 1 + 5 * X + error    # Y variable
lm1   <- summary(lm(Y~X))     # Correct OLS regression 
lm2   <- summary(lm(Y~X+X_ir))# OLS regression with X_ir 
lm1$r.squared < lm2$r.squared


## ---- fig.align="center"----------------------------------------------------------------
set.seed(2)
n      <- 100
K      <- 3
X      <- matrix(runif(n*(K-1), 2, 10), n, K-1)
X      <- cbind(1,X)
beta   <- c(1,5,5)
# heteroscedastic errors:
sigma  <- abs(X[,2] + X[,3])^1.5
error  <- rnorm(n, mean = 0, sd=sigma)
Y      <- beta[1]*X[,1] + beta[2]*X[,2] + beta[3]*X[,3] + error
##
lm_fit <- lm(Y~X -1 )
## Caution! By default R computes the standard errors 
## assuming homoscedastic errors. This can lead to 
## false inferences under heteroscedastic errors.
summary(lm_fit)$coefficients 

library("sandwich") # HC robust variance estimation 
library("lmtest")
## Robust estimation of the variance of \hat{\beta}:
Var_beta_hat_robust <- sandwich::vcovHC(lm_fit, type="HC3")
Var_beta_hat_robust 
## Corresponding regression-output:
lmtest::coeftest(lm_fit, vcov = Var_beta_hat_robust)


## ---------------------------------------------------------------------------------------
## install.packages("AER")
library("AER") ## load the R package
data(CPS1988)  ## attach the data


## ---------------------------------------------------------------------------------------
summary(CPS1988)


## ---------------------------------------------------------------------------------------
cps_lm <- lm(log(wage) ~ experience + I(experience^2) + 
               education + ethnicity, data = CPS1988)


## ---------------------------------------------------------------------------------------
## Regression output without robust standard errors (SEs):
## summary(cps_lm) ## Generally, do not do this.

## But do this: 
## Heteroscedasticity robust variance estimation: 
library("sandwich") 
library("lmtest")
## Robust estimation of the variance of \hat{\beta}:
Var_beta_hat_robust <- sandwich::vcovHC(cps_lm, type="HC3")
## Regression output table with robust SEs:
lmtest::coeftest(cps_lm, vcov = Var_beta_hat_robust)


## ---------------------------------------------------------------------------------------
cps_lm_2 <- lm(log(wage) ~ experience + I(experience^2) + 
                 education*ethnicity, data = CPS1988)
## Robust estimation of the variance of \hat{\beta}:
Var_beta_hat_robust <- sandwich::vcovHC(cps_lm_2, type="HC3")
## Regression output table with robust SEs:
lmtest::coeftest(cps_lm_2, vcov = Var_beta_hat_robust)

