#Lasso Test Run:
library(dplyr)
library(glmnet)

#inputs: indicators_2
#output: pct_stayed

output <- indicators_2$pct_stayed
inputs <- indicators_2 %>%
  select(-pct_stayed)

l1<- cv.glmnet(inputs, output, alpha=1) #lasso
l2<- cv.glmnet(inputs, output, alpha=0) #ridge
l12<- cv.glmnet(inputs, output, alpha=0.5) #elnet
names(l1)
l1$lambda.min; l1$lambda.1se; 
l2$lambda.min; l2$lambda.1se; 
l12$lambda.min; l12$lambda.1se; 

# use l1$lambda.min
fit.lasso1 <- glmnet(inputs, output, family="gaussian", 
                     lambda=l1$lambda.min, alpha=1)
fit.ridge1 <- glmnet(inputs, output, family="gaussian", 
                     lambda=l2$lambda.min, alpha=0)
fit.lasso2<- glmnet(inputs, output, family="gaussian", 
                    lambda=l1$lambda.1se, alpha=1)
# use l1$lambda.1se (1-standard-error rule)
fit.elnet1 <- glmnet(inputs, output, family="gaussian", 
                     lambda=l12$lambda.min, alpha=.5)
fit.ridge2 <- glmnet(inputs, output, family="gaussian", 
                     lambda=l2$lambda.1se, alpha=0)
fit.elnet2 <- glmnet(inputs, output, family="gaussian", 
                     lambda=l12$lambda.1se, alpha=.5)

#names(fit.lasso1)
cbind(fit.lasso1$beta, fit.lasso2$beta)
cbind(fit.ridge1$beta, fit.ridge2$beta)
cbind(fit.elnet1$beta, fit.elnet2$beta)

# examine a grid of lambda's
fit.lasso <- glmnet(inputs, output, family="gaussian", alpha=1)
fit.ridge <- glmnet(inputs, output, family="gaussian", alpha=0)
fit.elnet <- glmnet(inputs, output, family="gaussian", alpha=.5)
par(mfrow=c(3,2), mar=c(4,4,3,1))
plot(fit.lasso, "norm", label=TRUE); plot(fit.lasso, "lambda", label=TRUE); 
plot(fit.ridge, "norm", label=TRUE); plot(fit.ridge, "lambda", label=TRUE); 
plot(fit.elnet, "norm", label=TRUE); plot(fit.ridge, "lambda", label=TRUE)