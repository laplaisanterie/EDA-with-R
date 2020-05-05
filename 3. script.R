#1.
data = read.table("DISTRESS.dat", header=FALSE)
data = as.vector(as.matrix(data))

#1.가.
library(stringr)
data2 = str_replace_all(data, "\\*", "")
data2 = as.numeric(data2)
data2

fivenum(data2)

re_exp = function(x){
  Hl <- fivenum(x)[2]
  M <- fivenum(x)[3]
  Hu <- fivenum(x)[4]
  return(1-2*M*(Hu-M+Hl-M)/((Hl-M)^2+(Hu-M)^2))
}

re_exp(data2)

stem(data2)
stem(log(data2))

paste("원자료 평균:", as.character(mean(data2)))
paste("원자료 중앙값:", as.character(median(data2)))
paste("로그 변환 자료 평균:", as.character(mean(log(data2))))
paste("로그 변환 자료 중앙값:", as.character(median(log(data2))))

par(mfrow=c(1,2))
boxplot(data2, col="papayawhip", ylab="몸무게")
title("Boxplot of Weights")
boxplot(log(data2), col="papayawhip", ylab="로그몸무게")
title("Boxplot of Log Weights")

skewness = function(x){
  Hl <- fivenum(x)[2]
  M <- fivenum(x)[3]
  Hu <- fivenum(x)[4]
  return((Hu-M-M+Hl)/(Hu-M+M-Hl))
}

# 원자료 왜도
skewness(data2)
# 로그변환 자료 왜도 
skewness(log(data2))

#1.나
label = c(rep("사망", 27), rep("생존", length(data)-27))

df = data.frame(data2, label)
colnames(df) = c("몸무게", "생존여부" )
head(df)

re_exp(df[df$생존여부=="생존",]$몸무게)
re_exp(df[df$생존여부=="사망",]$몸무게)

HsprA <- fivenum(df[df$생존여부=="사망",]$몸무게)[4] - fivenum(df[df$생존여부=="사망",]$몸무게)[2]
HsprB <- fivenum(df[df$생존여부=="생존",]$몸무게)[4] - fivenum(df[df$생존여부=="생존",]$몸무게)[2]
M <- c(median(df[df$생존여부=="사망",]$몸무게),  median(df[df$생존여부=="생존",]$몸무게))
Hspr = c(HsprA, HsprB)

(RegrLine <- lm(log(Hspr) ~ log(M)))       # ( ) is needed to print coeff's

par(mfrow=c(1,1))
plot(log(M), log(Hspr), main="Spread vs. Level plot")
abline(coef(RegrLine))

HsprA <- fivenum(df[df$생존여부=="사망",]$몸무게)[4] - fivenum(df[df$생존여부=="사망",]$몸무게)[2]
HsprB <- fivenum(df[df$생존여부=="생존",]$몸무게)[4] - fivenum(df[df$생존여부=="생존",]$몸무게)[2]

HsprA - HsprB

HsprC <- fivenum(df[df$생존여부=="사망",]$몸무게^0.5)[4] - fivenum(df[df$생존여부=="사망",]$몸무게^0.5)[2]
HsprD <- fivenum(df[df$생존여부=="생존",]$몸무게^0.5)[4] - fivenum(df[df$생존여부=="생존",]$몸무게^0.5)[2]

HsprC - HsprD

par(mfrow=c(1,2))
boxplot(몸무게 ~ 생존여부, data=df, col="papayawhip", notch=T)
title("Boxplot of Weights")
df$루트몸무게 = sqrt(df$몸무게)
boxplot(루트몸무게 ~ 생존여부, data=df, col="papayawhip", notch=T)
title("Boxplot of Square root Weights")

#2. 
fivenum(airquality$Ozone, na.rm=T)
re_exp(airquality$Ozone)

par(mfrow=c(1,2))
boxplot(airquality$Ozone, na.rm=T, col="papayawhip")
title("Boxplot of Ozone")
boxplot(log(airquality$Ozone), na.rm=T, col="papayawhip")
title("Boxplot of Log Ozone")

paste("원자료 평균:", as.character(mean(airquality$Ozone, na.rm=TRUE)))
paste("원자료 중앙값:", as.character(median(airquality$Ozone, na.rm=TRUE)))
paste("로그 변환 자료 평균:", as.character(mean(log(airquality$Ozone), na.rm=TRUE)))
paste("로그 변환 자료 중앙값:", as.character(median(log(airquality$Ozone), na.rm=TRUE)))

# 원자료 왜도
skewness(airquality$Ozone)
# 로그 변환 자료 왜도 
skewness(log(airquality$Ozone))

#3.
#Load Data
library(rvest)
library(httr)
library(stringr)
ticker=list()
for (i in 1:150) {
url=paste0("https://finance.naver.com/item/sise_day.nhn?code=005930&page=",i)
down = GET(url)

Sys.setlocale("LC_ALL", "English")

table = read_html(down, encoding = "EUC-KR") %>%
  html_table(., fill = TRUE)
table = table[[1]]
table = table[-c(1,7,8,9,15),]

Sys.setlocale("LC_ALL", "Korean")

ticker[[i]] = table
}

ticker = do.call(rbind, ticker)
rownames(ticker) = NULL

ticker_2016 = ticker[grep("^2016", ticker$날짜),]
rownames(ticker_2016) = NULL

par(mfrow=c(1,1))
price = as.numeric(str_replace_all(ticker_2016$종가, ",",""))
print(head(price))

#stem and boxplot
par(mfrow=c(1,1))
stem(price)
boxplot(price, col="papayawhip", ylim=c(1000000, 2000000), ylab="price(won)")
title("Boxplot of Samsung Electronics' stock price(2016)")

p = seq(-5, 5, by=0.1)
skewness_p = c()
for (i in p){
  n = skewness(price^i)
  skewness_p = c(skewness_p, n)
}

# p=0에 대해 log값을 넣어준다.
skewness_p[51] = skewness(log(price))

# 절대값을 취한다.
skewness_p = abs(skewness_p)

plot(p, skewness_p, type="l", ylab="Skewness(abs)")
title("p의 변화에 따른 왜도(절대값)의 변화")

p[which(skewness_p == min(skewness_p))]

#원자료
stem(price)
#재표현자료
stem(price^3.4)

par(mfrow=c(1,2))
boxplot(price, col="papayawhip", ylim=c(1000000, 2000000), ylab="price(won)")
title("Boxplot of Samsung Electronics' stock price(2016)")
boxplot(price^3.4, col="papayawhip", ylab="price(won)")
title("Boxplot of Re-expressed Samsung Electronics' stock price(2016)")

s1 = abs(skewness(price))
s2 = abs(skewness(log(price)))
s3 = abs(skewness(price^0.5))
s4 = abs(skewness(price^-1))
s5 = abs(skewness(price^-0.5))
s6 = abs(skewness(price^2))

s = c(s1, s2, s3, s4, s5, s6)
s = round(s, 2)
s = as.character(s)

par(mfrow=c(2,3))
boxplot(price, col="papayawhip", ylab="price(won)")
title("original")
text(x= 0.8, y= 1700000, labels= paste("왜도:",s[1]), cex=2)
boxplot(log(price), col="papayawhip", ylab="price(won)")
title("log")
text(x= 0.8, y= 14.35, labels= paste("왜도:",s[2]), cex=2)
boxplot(price^0.5, col="papayawhip", ylab="price(won)")
title("p=0.5")
text(x= 0.8, y= 1300, labels= paste("왜도:",s[3]), cex=2)
boxplot(-price^-1, col="papayawhip", ylab="price(won)")
title("p=-1")
text(x= 0.8, y= -6.0e-07, labels=paste("왜도:",s[4]), cex=2)
boxplot(-price^-0.5, col="papayawhip", ylab="price(won)")
title("p=-0.5")
text(x= 0.8, y= -0.000765, labels= paste("왜도:",s[5]), cex=2)
boxplot(price^2, col="papayawhip", ylab="price(won)")
text(x= 0.8, y= 2.8e+12, labels= paste("왜도:",s[6]), cex=2)
title("p=2")
