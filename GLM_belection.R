library(GLMsData)
library(MASS)
library(statmod)

data(belection)
str(belection)
belection$Total = belection$Females+belection$Males

plot(Females/Total ~ Party, data=belection)
plot(Females/Total ~ Region, data=belection, cex=.5, las=2)

model1 = glm(cbind(Females, Total-Females) ~ Party+Region,
             family=binomial, data=belection)
summary(model1)

model2 = glm(cbind(Females, Total-Females) ~ Party,
             family=binomial, data=belection)
summary(model2)
anova(model2, model1, test="Chisq")

qres2 = qresid(model2)
qqnorm(qres2, las=1, main="Q-Q plot for model2")
abline(0, 1)
scatter.smooth(qres2~fitted(model2), las=1, main="Residuals vs fitted for model2",
               xlab="Fitted value", ylab="Quantile residual")

model2_q = update(model2, family=quasibinomial)
summary(model2_q)$dispersion

Cons = exp(coef(model2)[1])
Cons

Lab = exp(coef(model2)[1]+coef(model2)[3])
Lab

Cons/Lab
