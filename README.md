#####RIDGE######
EEO = read.table("C:/Users/DELL/Downloads/Ridge_Lasso_R_Practical_Data.txt", h=T)
library(MASS)
lm.ridge(ACHV~FAM + PEER + SCHOOL,data=EEO,lambda = c(1,5,10))
EEO.rg = lm.ridge(ACHV ~ FAM + PEER + SCHOOL, data=EEO,
                  lambda=10^seq(1.5, -2, by = -.1))
par(mai=c(0.6,0.6,0.01,0.01), mgp=c(2,0.7,0))
plot(EEO.rg)
matplot(EEO.rg$lambda, coef(EEO.rg), type = "l", lwd=2,
        xlab = "lambda", ylab = "coefficients")
plot(EEO.rg$lambda, EEO.rg$GCV, type = 'l',
     xlab = "lambda", ylab = "GCV Score")
select(EEO.rg)        
pre=lm.ridge(ACHV ~ FAM + PEER + SCHOOL, data=EEO, lambda=19.95)
summary(pre)
lm.ridge(ACHV~FAM + PEER + SCHOOL,data=EEO)$coef
######LASSO#######
install.packages("lars")
meatspec = read.table("http://www.stat.uchicago.edu/~yibi/s224/data/meatspec.txt",header=TRUE)
meatspec
summary(meatspec)
train=meatspec[1:172,]
test=meatspec[173:215,]
trainy = train$fat
trainx = as.matrix(train[,-101])
library(lars)
lassomod = lars(trainx,trainy)
plot(lassomod)
set.seed(235)
cvout =cv.lars(trainx, trainy)
cvout$index[which.min(cvout$cv)]
pred=predict(lassomod,s=0.01010101,type='coef',mode="fraction")
pred
pred$coefficients
sum(pred$coefficients!=0)
pred$coefficients[pred$coefficients != 0]







