
#-----------------------------------------------------------------------------------------------------#
#							Outlier detection (mahalanobis)
#-----------------------------------------------------------------------------------------------------#
df=iris[,1:4]

# calculate mahalanobis distance
m_dist <- mahalanobis(df, colMeans(df), cov(df))

#  calculate cutoff based on 0.001 chi-squared test
Cutoff = qchisq(1-0.001,ncol(df))

# degrees of freedom
ncol(df)

# check if samples failed
summary(m_dist < Cutoff)

# get data which passed
df[m_dist < Cutoff,]


#-----------------------------------------------------------------------------------------------------#
#							Check multicolinearity/additivity
#-----------------------------------------------------------------------------------------------------#
# corrlation of variables
correlation = cor(df,use = "pairwise.complete.obs")
correlation

# display matrix of positive correlations
symnum(correlation)


#-----------------------------------------------------------------------------------------------------#
#							Check assumptions
#-----------------------------------------------------------------------------------------------------#
# these are the things we need to check it
random = rchisq(nrow(df),7)
fake= lm(random~.,data=df)
fitted = scale(fake$fitted.values)
standardized = rstudent(fake) # standardized residuals

# linarity
qqnorm(standardized)
abline(0,1)

# Normality
hist(standardized)

# homos
plot(fitted,standardized)
abline(h=0)
abline(v=0)


#-----------------------------------------------------------------------------------------------------#
#							Check models
#-----------------------------------------------------------------------------------------------------#
# intercept only
model1 = nlme::gls(Petal.Width ~ 1, # tests if the intercept is sign diff from 0
	data=df,
	method="ML",
	na.action="na.omit")
summary(model1)


# random intercept only
model2 = nlme::lme(Petal.Width ~ 1,
	data=iris,
	method="ML",
	na.action="na.omit",
	random=~1|Species)
summary(model2)

# to compare whcih model is better
# NOTE: does not run a 'real' anova, just to compare values
# Evaluate: which AIC/BIC is lower. Lower is better.
anova(model1,model2)

# >       Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# > model1     1  2 347.2292 353.2504 -171.6146                        
# > model2     2  3 -24.8702 -15.8383   15.4351 1 vs 2 374.0994  <.0001

# as can be seen model 2 is better (and sign)
# more complex controlling can be done by

# random intercept only
#model3 = nlme::lme(Petal.Width ~ 1,
#	data=iris,
#	method="ML",
#	na.action="na.omit",
#	random=list(~1|Species,~1|Sepal.Length)) # added sepal length as example
#summary(model3)
#anova(model1,model2,model3) # model 3 not significant

# THUS: controlling for 1 item (model 2)

# Add in variable of interest to chosen model

# random intercept only
model3 = nlme::lme(Petal.Width ~ Petal.Length,
	data=iris,
	method="ML",
	na.action="na.omit",
	random=~1|Species)
summary(model3)
anova(model1,model2,model3)
#       Model df      AIC      BIC     logLik   Test  L.Ratio p-value
#model1     1  2 347.2292 353.2504 -171.61458                        
#model2     2  3 -24.8702 -15.8383   15.43510 1 vs 2 374.0994  <.0001
#model3     3  4 -67.0362 -54.9936   37.51808 2 vs 3  44.1660  <.0001

# model 3 is better! also when checking the summary of model 3  the correlation between Petal length and intercept(random) implies some possible structure. Possibly explained by random slopes?
#
#model4 = nlme::lme(Petal.Width ~ Petal.Length,
#	data=iris,
#	method="ML",
#	na.action="na.omit",
#	random=~Petal.Length|Species,
#	control= nlme::lmeControl(msMaxIter=200)) # else it doesnt converge
#summary(model4)
#anova(model1,model2,model3,model4)#
# model 4 doesnt converge; saying at model 3 it is!

#Now check the assumptions for the final model

standardized = as.data.frame(scale(model3$residuals))
standardized = standardized$fixed
fitted = fitted.values(model3)

# linarity
qqnorm(standardized)
abline(0,1)

# Normality
hist(standardized)

# homos
plot(fitted,standardized)
abline(h=0)
abline(v=0)


#-----------------------------------------------------------------------------------------------------#
#							Check phenotypic features
#-----------------------------------------------------------------------------------------------------#
# The idea here is that the phenotypic features have some overlap (think about subtests and a sum). These should have common variance. Using EFA, the common variance is assessed. First the correlations of each feature are evaluated. Second the Kaiser-Meyer-Olkin (KMO) index is calculated, it evaluates the amount of compare variance to unique variance 

pheno=iris[,-5]


pheno = round(pheno,2)

cor(pheno)
psych::KMO(cor(pheno))

# factor analysis
psych::fa(iris[,-5], nfactors=2,rotate="oblimin",fm="ml")
#ML1   ML2   h2     u2 com
#Sepal.Length  1.01  0.30 1.00 0.0050 1.2
#Sepal.Width  -0.18  0.61 0.46 0.5448 1.2
#Petal.Length  0.93 -0.21 1.00 0.0049 1.1
#Petal.Width   0.88 -0.25 0.93 0.0676 1.2

# this shows factor 1 is S.L + P.L + P.Width
# factor 2: S.Width
# lets plot that

x = iris$Sepal.Length+iris$Petal.Length+iris$Petal.Width
y = iris$Sepal.Width
plot(x,y,col=iris$Species)






#pheno=mtcars[,c(4,8,9,10,11)]
pheno=mtcars[,c(5,7,8,9,11)]
pheno = round(pheno,2)

cor(pheno)
psych::KMO(cor(pheno))

# get basic idea of amount of fators required:
plot(svd(cor(pheno))$d)

# factor analysis
res_fa = psych::fa(pheno, nfactors=2,rotate="oblimin",fm="ml")
res_fa



factor1 = which(abs(res_fa$loadings[,"ML1"])>0.3 & abs(res_fa$loadings[,"ML2"])<0.3 & abs(res_fa$loadings[,"ML3"])<0.3)
factor2 = which(abs(res_fa$loadings[,"ML2"])>0.3 & abs(res_fa$loadings[,"ML1"])<0.3 & abs(res_fa$loadings[,"ML3"])<0.3)
factor3 = which(abs(res_fa$loadings[,"ML3"])>0.3 & abs(res_fa$loadings[,"ML2"])<0.3 & abs(res_fa$loadings[,"ML1"])<0.3)

names(factor1)
names(factor2)
names(factor3)

psych::alpha( pheno[,names(factor1)])
psych::alpha( pheno[,names(factor2)])
psych::alpha( pheno[,names(factor3)])

x = pheno[,names(factor1)]
y = pheno[,names(factor2)]
plot(x,y)