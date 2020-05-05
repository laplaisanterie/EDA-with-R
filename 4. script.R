#1.
score = c(14, 11, 13, 13, 13, 15, 11, 16, 10,
          13, 14, 11, 13, 12, 10, 14, 10, 14,
          16, 14, 14, 11, 11, 11, 13, 12, 13,
          11, 11, 15, 14, 16, 12, 17,  9, 16,
          11, 19, 14, 12, 12, 10, 11, 12, 13,
          13, 14, 11, 11, 15, 12, 16, 15, 11)

qqnorm(score, ylab="Scores of Students",main="Normal Prob Plot")

qqnorm(score, ylab="Scores of Students",main="Normal Prob Plot")
#qqline(score)

score.x = quantile(score, probs=c(0.25,0.75), names = FALSE,na.rm = TRUE)
score.y = qnorm(c(0.25,0.75))

slope = diff(score.x)/diff(score.y)
int = score.x[1] - slope * score.y[1]

abline(int, slope, col="green2", lwd=3)

x = fivenum(score)
pseudosigma = (x[4]-x[2])/1.34
abline(x[3],pseudosigma, col="red", lwd=3)

legend(-2, 18, c("qqline", "EDA line"), col=c("green2", "red"), lty=1, lwd=3)

slope #초록선의 기울기
pseudosigma #빨간선의 기울기
int #초록선의 절편
x[3] #빨간선의 절편

#skewness
skewness = function(x){
  Hl <- fivenum(x)[2]
  M <- fivenum(x)[3]
  Hu <- fivenum(x)[4]
  return((Hu-M-M+Hl)/(Hu-M+M-Hl))
}

skewness(score)

#density function
hist(score, freq = FALSE, ylim=c(0,0.3))
lines(density(score), lwd=2)
n = rnorm(100000, x[3], pseudosigma)
lines(density(n), col="red", lwd=2)
legend(18, 0.28, c("Score", "Normal"), col=c("black", "red"), lty=1, lwd=2)

#Kolmogorov-Smirnov test
ks.test(score, "pnorm", x[3], pseudosigma)
#Cramer-von Mises test
library(goftest)
cvm.test(score, "pnorm", x[3], pseudosigma)
#Anderson-Darling test
ad.test(score, "pnorm", x[3], pseudosigma)


#2. 
monopoly = c(4.65,  4.55,  4.11,  4.15,  4.20,  4.55,  3.80,  4.00, 4.19,  4.75,
             4.74,  4.50,  4.10,  4.00,  5.05,  4.20)
private = c(4.82,  5.29,  4.89,  4.95,  4.55,  4.90,  5.25,  5.30,  4.29,  4.85,
            4.54,  4.75,  4.85,  4.85,  4.50,  4.75,  4.79,  4.85,  4.79,  4.95,
            4.95,  4.75,  5.20,  5.10,  4.80,  4.29)
length(monopoly)
length(private)

qqplot(monopoly, private)
line(qqplot(monopoly, private))
abline(line(qqplot(monopoly, private)))
title("qqplot ; monopoly states vs private-ownership states")


#tukey mean-diff
qq.x = qqplot(monopoly,private)$x
qq.y = qqplot(monopoly,private)$y
plot((qq.x+qq.y)/2, qq.y-qq.x, ylim=c(-1, 1),
     ylab="Private - Monopoly", xlab="mean")
abline(0,0)
title("Tukey's Mean Difference Plot")

#Kolmogorov-Smirnov test
ks.test(monopoly, private, alternative="two.sided")
ks.test(monopoly+0.5, private, alternative="two.sided")

#AUC
library(pROC)
ylab=factor(c(rep(0,length(monopoly)), rep(1,length(private))))
res = roc(ylab, c(monopoly, private))
res.ad = roc(ylab, c(monopoly+0.5, private))

par(mfrow=c(1,2))
plot(res, xlim=c(1,0), main="ROC between monopoly and private")
plot(res.ad, xlim=c(1,0), main="ROC between monopoly+0.5 and private")

#왼쪽그림의 AUC
auc(res) 
#오른쪽그림의 AUC
auc(res.ad)

#3.
x = c(rep(32,10),rep(33,23),rep(34,48),rep(35,80),rep(36,63),
      rep(37,65),rep(38,47),rep(39,33),rep(40,14),rep(42,6))

par(mfrow=c(2,2))
qqnorm(x, main='Normal prob plot')

n = length(x)
i = 1:n
q.exp.x = -log(1-(i-0.5)/n)          
plot(q.exp.x, x, main="Exponential prob plot")
plot(q.exp.x^(1/3), x^(1/3),main="Exponential; power=1/3")

q.weibull.x = log(q.exp.x)
plot(q.weibull.x, log(x), main="Weibull prob plot")

score.x = quantile(x, probs=c(0.25,0.75), names = FALSE,na.rm = TRUE)
score.y = qnorm(c(0.25,0.75))

slope = diff(score.x)/diff(score.y)
int = score.x[1] - slope * score.y[1]

slope
int

par(mfrow=c(1,1))
qqnorm(x, main='Normal prob plot')
abline(int, slope, lwd=2)

par(mfrow=c(1,1))
plot(q.weibull.x, log(x), main="Weibull prob plot")
abline(3.61148,0.04876, lwd=2)

(b=1/0.04876)
(a=exp(3.61148*(-b)))