---
  title: "Untitled"
author: "Qiwei Guo"
date: "2022/2/19"
output: html_document
---
  
  ```{r,echo=FALSE,include=FALSE}
##import the data set
library(readxl)
library("writexl")
library("xtable")
Data <- read_excel("Data.xlsx")
attach(Data)
```

```{r,echo=FALSE,include=FALSE}
##calculate face value and last coupon payment
fv<-100 + 100*coupon
##calculate time to maturity
maturity_date<-as.Date(`maturity date`,format = "%m/%d/%Y")
time_till_maturity<-difftime(maturity_date,"2022-1-29",units = "days")
ttm<-as.integer(time_till_maturity)/365
```

```{r,echo=FALSE,include=FALSE}
##time until next coupon payment:
dtm<-1-(ttm-as.integer(ttm))
##calculate the dirty price
Accured_interest<-dtm*coupon*100
dp_jan10<-Accured_interest+cp_jan10
dp_jan11<-Accured_interest+cp_Jan11
dp_jan12<-Accured_interest+cp_jan12
dp_jan13<-Accured_interest+cp_jan13
dp_jan14<-Accured_interest+cp_jan14
dp_jan17<-Accured_interest+cp_jan17
dp_jan18<-Accured_interest+cp_jan18
dp_jan19<-Accured_interest+cp_jan19
dp_jan20<-Accured_interest+cp_jan20
dp_jan21<-Accured_interest+cp_jan21
dp_jan24<-Accured_interest+cp_jan24
```

```{r,echo=FALSE}
# Create cash flow vectors
cf_1<-c(1, 100)
cf_1=cf_1+coupon[1]*100
cf_2<-c(2,0,100)
cf_2=cf_2+coupon[2]*100
cf_3<-c(3,0,0,100)
cf_3=cf_3+coupon[3]*100
cf_4<-c(4,0,0,0,100)
cf_4=cf_4+coupon[4]*100
cf_5<-c(5,0,0,0,0,100)
cf_5=cf_5+coupon[5]*100
cf_6<-c(6,0,0,0,0,0,100)
cf_6=cf_6+coupon[6]*100
cf_7<-c(7,0,0,0,0,0,0,100)
cf_7=cf_7+coupon[7]*100
cf_8<-c(8,0,0,0,0,0,0,0,100)
cf_8=cf_8+coupon[8]*100
cf_9<-c(9,0,0,0,0,0,0,0,0,100)
cf_9=cf_9+coupon[9]*100
cf_10<-c(10,0,0,0,0,0,0,0,0,0,100)
cf_10=cf_10+coupon[10]*100
cf_list<-list(cf_1,cf_2,cf_3,cf_4,cf_5,cf_6,cf_7,cf_8,cf_9,cf_10)

```


```{r,echo=FALSE}
# Create bond valuation function(PV)
bval <- function(i, cf,
                 t=seq(along = cf))
  sum(cf / (1 + i)^t)

# Create ytm() function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

```

```{r,echo=FALSE}
##create a function to process a vector at a time
yield=c(1:10)
ytm_c<-function(dirty_price){
  for (i in (1:10)) {
    cf<-unlist(cf_list[i])
    cf[1]= -dirty_price[i]
    yield[i]=ytm(cf)
  }
  return(yield)
}
```


```{r,echo=FALSE}
##calculate ytm's
Jan_10_ytm<-ytm_c(dp_jan10)
Jan_11_ytm<-ytm_c(dp_jan11)
Jan_12_ytm<-ytm_c(dp_jan12)
Jan_13_ytm<-ytm_c(dp_jan13)
Jan_14_ytm<-ytm_c(dp_jan14)
Jan_17_ytm<-ytm_c(dp_jan17)
Jan_18_ytm<-ytm_c(dp_jan18)
Jan_19_ytm<-ytm_c(dp_jan19)
Jan_20_ytm<-ytm_c(dp_jan20)
Jan_21_ytm<-ytm_c(dp_jan21)
Jan_24_ytm<-ytm_c(dp_jan24)
```

```{r,echo=TRUE}
##plot the ytm curve
xvalue<-c(1:10)/2
pdf("ytm_curve.pdf")
plot(xvalue,Jan_10_ytm*100,type = "b", col = 2 , lwd = 3, pch = 1, main = "yield to maturity",xlab = "time till maturity",ylab = "yield to maturity")
lines(xvalue, Jan_11_ytm*100, type = "b", col = 3 , lwd = 3, pch = 1)
lines(xvalue, Jan_12_ytm*100, type = "b", col = 4 , lwd = 3, pch = 1)
lines(xvalue, Jan_13_ytm*100, type = "b", col = 5 , lwd = 3, pch = 1)
lines(xvalue, Jan_14_ytm*100, type = "b", col = 6 , lwd = 3, pch = 1)
lines(xvalue, Jan_17_ytm*100, type = "b", col = 7 , lwd = 3, pch = 1)
lines(xvalue, Jan_18_ytm*100, type = "b", col = 8 , lwd = 3, pch = 1)
lines(xvalue, Jan_19_ytm*100, type = "b", col = 9 , lwd = 3, pch = 1)
lines(xvalue, Jan_20_ytm*100, type = "b", col = 10 , lwd = 3, pch = 1)
lines(xvalue, Jan_21_ytm*100, type = "b", col = 11 , lwd = 3, pch = 1)
lines(xvalue, Jan_24_ytm*100, type = "b", col = 12 , lwd = 3, pch = 1)
legend("bottomright", legend = c("Jan10", "Jan11", "Jan12","Jan13","Jan14","Jan17","Jan18","Jan19","Jan20","Jan21","Jan24"),
       lwd = 3, col = c(3:12))
```

```{r,echo=FALSE}
##calculate the value of previous n-1 terms include the present price
coupon_payment = 100 *coupon
r<-c(1:10)

r1<-function(dp){
  -log(dp[1]/(100+coupon_payment[1]))/ttm[1]
}

r2<-function(dp,r_1=r1(dp)){
  -log((dp[2]-(coupon_payment[2]*exp(-r_1*xvalue[1])))/(100+coupon_payment[2]))/xvalue[2]
}

r3<-function(dp,r_1=r1(dp),r_2=r2(dp)){
  -log((dp[3]-(coupon_payment[3]*exp(-r_1*xvalue[1]))-(coupon_payment[3]*exp(-r_2*xvalue[2])))/(100+coupon_payment[3]))/xvalue[3]
}

r4<-function(dp,r_1=r_1(dp),r_2=r_2(dp),r_3=r3(dp)){
  -log((dp[4]-(coupon_payment[4]*exp(-r[1]*xvalue[1]))-(coupon_payment[4]*exp(-r[2]*xvalue[2]))-(coupon_payment[4]*exp(-r[3]*xvalue[3])))/(100+coupon_payment[4]))/xvalue[4]
}

r5<-function(dp,r_1=r_1(dp),r_2=r_2(dp),r_3=r3(dp),r_4=r4(dp)){
  -log((dp[5]-(coupon_payment[5]*exp(-r[1]*xvalue[1]))-(coupon_payment[5]*exp(-r[2]*xvalue[2]))-(coupon_payment[5]*exp(-r[3]*xvalue[3]))-(coupon_payment[5]*exp(-r[4]*xvalue[4])))/(100+coupon_payment[5]))/xvalue[5]
}

r6<-function(dp,r_1=r_1(dp),r_2=r_2(dp),r_3=r3(dp),r_4=r4(dp),r_5=r5(dp)){
  -log((dp[6]-(coupon_payment[6]*exp(-r[1]*xvalue[1]))-(coupon_payment[6]*exp(-r[2]*xvalue[2]))-(coupon_payment[6]*exp(-r[3]*xvalue[3]))-(coupon_payment[6]*exp(-r[4]*xvalue[4]))-(coupon_payment[6]*exp(-r[5]*xvalue[5])))/(100+coupon_payment[6]))/xvalue[6]
}

r7<-function(dp,r_1=r_1(dp),r_2=r_2(dp),r_3=r3(dp),r_4=r4(dp),r_5=r5(dp),r_6=r6(dp)){
  -log((dp[7]-(coupon_payment[7]*exp(-r[1]*xvalue[1]))-(coupon_payment[7]*exp(-r[2]*xvalue[2]))-(coupon_payment[7]*exp(-r[3]*xvalue[3]))-(coupon_payment[7]*exp(-r[4]*xvalue[4]))-(coupon_payment[7]*exp(-r[5]*xvalue[5]))-(coupon_payment[7]*exp(-r[6]*xvalue[6])))/(100+coupon_payment[7]))/xvalue[7]
}

r8<-function(dp,r_1=r_1(dp),r_2=r_2(dp),r_3=r3(dp),r_4=r4(dp),r_5=r5(dp),r_6=r6(dp),r_7=r7(dp)){
  -log((dp[8]-(coupon_payment[8]*exp(-r[1]*xvalue[1]))-(coupon_payment[8]*exp(-r[2]*xvalue[2]))-(coupon_payment[8]*exp(-r[3]*xvalue[3]))-(coupon_payment[8]*exp(-r[4]*xvalue[4]))-(coupon_payment[8]*exp(-r[5]*xvalue[5]))-(coupon_payment[8]*exp(-r[6]*xvalue[6]))-(coupon_payment[8]*exp(-r[7]*xvalue[7])))/(100+coupon_payment[8]))/xvalue[8]
}

r9<-function(dp,r_1=r_1(dp),r_2=r_2(dp),r_3=r3(dp),r_4=r4(dp),r_5=r5(dp),r_6=r6(dp),r_7=r7(dp),r_8=r8(dp)){
  -log((dp[9]-(coupon_payment[9]*exp(-r[1]*xvalue[1]))-(coupon_payment[9]*exp(-r[2]*xvalue[2]))-(coupon_payment[9]*exp(-r[3]*xvalue[3]))-(coupon_payment[9]*exp(-r[4]*xvalue[4]))-(coupon_payment[9]*exp(-r[5]*xvalue[5]))-(coupon_payment[9]*exp(-r[6]*xvalue[6]))-(coupon_payment[9]*exp(-r[7]*xvalue[7]))-(coupon_payment[9]*exp(-r[8]*xvalue[8])))/(100+coupon_payment[9]))/xvalue[9]
}

r10<-function(dp,r_1=r_1(dp),r_2=r_2(dp),r_3=r3(dp),r_4=r4(dp),r_5=r5(dp),r_6=r6(dp),r_7=r7(dp),r_8=r8(dp),r_9=r9(dp)){
  -log((dp[10]-(coupon_payment[10]*exp(-r[1]*xvalue[1]))-(coupon_payment[10]*exp(-r[2]*xvalue[2]))-(coupon_payment[10]*exp(-r[3]*xvalue[3]))-(coupon_payment[10]*exp(-r[4]*xvalue[4]))-(coupon_payment[10]*exp(-r[5]*xvalue[5]))-(coupon_payment[10]*exp(-r[6]*xvalue[6]))-(coupon_payment[10]*exp(-r[7]*xvalue[7]))-(coupon_payment[10]*exp(-r[8]*xvalue[8]))-(coupon_payment[10]*exp(-r[9]*xvalue[9])))/(100+coupon_payment[10]))/xvalue[10]
}

spot<-function(dp){
  c<-r1(dp)
  c[2]=r2(dp)
  c[3]=r3(dp)
  c[4]=r4(dp)
  c[5]=r5(dp)
  c[6]=r6(dp)
  c[7]=r7(dp)
  c[8]=r8(dp)
  c[9]=r9(dp)
  c[10]=r10(dp)
  return(c)
}
```


```{r,echo=FALSE}
##calculate spot rate
Jan_10_spot<-spot(dp_jan10)
Jan_11_spot<-spot(dp_jan11)
Jan_12_spot<-spot(dp_jan12)
Jan_13_spot<-spot(dp_jan13)
Jan_14_spot<-spot(dp_jan14)
Jan_17_spot<-spot(dp_jan17)
Jan_18_spot<-spot(dp_jan18)
Jan_19_spot<-spot(dp_jan19)
Jan_20_spot<-spot(dp_jan20)
Jan_21_spot<-spot(dp_jan21)
Jan_24_spot<-spot(dp_jan24)
```

```{r,echo=FALSE}
##plot spot rate
pdf("spot_curve.pdf")
plot(xvalue,Jan_10_spot*100,type = "b", col = 2 , lwd = 3, pch = 1, main = "spot curve",xlab = "time till maturity",ylab = "zero rate")
lines(xvalue, Jan_11_spot*100, type = "b", col = 3 , lwd = 3, pch = 1)
lines(xvalue, Jan_12_spot*100, type = "b", col = 4 , lwd = 3, pch = 1)
lines(xvalue, Jan_13_spot*100, type = "b", col = 5 , lwd = 3, pch = 1)
lines(xvalue, Jan_14_spot*100, type = "b", col = 6 , lwd = 3, pch = 1)
lines(xvalue, Jan_17_spot *100, type = "b", col = 7 , lwd = 3, pch = 1)
lines(xvalue, Jan_18_spot*100, type = "b", col = 8 , lwd = 3, pch = 1)
lines(xvalue, Jan_19_spot*100, type = "b", col = 9 , lwd = 3, pch = 1)
lines(xvalue, Jan_20_spot*100, type = "b", col = 10 , lwd = 3, pch = 1)
lines(xvalue, Jan_21_spot*100, type = "b", col = 11 , lwd = 3, pch = 1)
lines(xvalue, Jan_24_spot*100, type = "b", col = 12 , lwd = 3, pch = 1)
legend("topright", legend = c("Jan10", "Jan11", "Jan12","Jan13","Jan14","Jan17","Jan18","Jan19","Jan20","Jan21","Jan24"),
       lwd = 3, col = c(3:12))
```

```{r,echo=FALSE}
forward_rate<-function(spotrate,j){
  ((1+spotrate[j])^xvalue[j])/(1+spotrate[2]) - 1
}

forward_curve<-function(sr){
  result = forward_rate(sr,4)
  result[2]=forward_rate(sr,6)
  result[3]=forward_rate(sr,8)
  result[4]=forward_rate(sr,10)
  return(result)
}
```

```{r,echo=FALSE}
Jan_10_forward<-forward_curve(Jan_10_spot)
Jan_11_forward<-forward_curve(Jan_11_spot)
Jan_12_forward<-forward_curve(Jan_12_spot)
Jan_13_forward<-forward_curve(Jan_13_spot)
Jan_14_forward<-forward_curve(Jan_14_spot)
Jan_17_forward<-forward_curve(Jan_17_spot)
Jan_18_forward<-forward_curve(Jan_18_spot)
Jan_19_forward<-forward_curve(Jan_19_spot)
Jan_20_forward<-forward_curve(Jan_20_spot)
Jan_21_forward<-forward_curve(Jan_21_spot)
Jan_24_forward<-forward_curve(Jan_24_spot)
```

```{r,echo=FALSE}
##plot forward rate
pdf("forward_curve.pdf")
plot((1:4),Jan_10_forward*100,type = "b", col = 2 , lwd = 3, pch = 1, main = "forward curve",xaxt = "n",ylab = "forward rate")
axis(1, at=1:4, labels=c("1yr-1yr","1yr-2yr","1yr-3yr","1yr-4yr"))
lines((1:4), Jan_11_forward*100, type = "b", col = 3 , lwd = 3, pch = 1)
lines((1:4), Jan_12_forward*100, type = "b", col = 4 , lwd = 3, pch = 1)
lines((1:4), Jan_13_forward*100, type = "b", col = 5 , lwd = 3, pch = 1)
lines((1:4), Jan_14_forward*100, type = "b", col = 6 , lwd = 3, pch = 1)
lines((1:4), Jan_17_forward*100, type = "b", col = 7 , lwd = 3, pch = 1)
lines((1:4), Jan_18_forward*100, type = "b", col = 8 , lwd = 3, pch = 1)
lines((1:4), Jan_19_forward*100, type = "b", col = 9 , lwd = 3, pch = 1)
lines((1:4), Jan_20_forward*100, type = "b", col = 10 , lwd = 3, pch = 1)
lines((1:4), Jan_21_forward*100, type = "b", col = 11 , lwd = 3, pch = 1)
lines((1:4), Jan_24_forward*100, type = "b", col = 12 , lwd = 3, pch = 1)
legend("topleft", legend = c("Jan10", "Jan11", "Jan12","Jan13","Jan14","Jan17","Jan18","Jan19","Jan20","Jan21","Jan24"),
       lwd = 3, col = c(3:12))
```



```{r,echo=FALSE}
random_variable<-c(1:5)
rv<-function(yield_1,yield_2){
  random_variable<-c(log(yield_2[2]/yield_1[2]))
  random_variable[2]=log(yield_2[4]/yield_1[4])
  random_variable[3]=log(yield_2[6]/yield_1[6])
  random_variable[4]=log(yield_2[8]/yield_1[8])
  random_variable[5]=log(yield_2[10]/yield_1[10])
  return(random_variable)
}
```

```{r,echo=FALSE}
x_1<-rv(Jan_10_ytm,Jan_11_ytm)
x_2<-rv(Jan_11_ytm,Jan_12_ytm)
x_3<-rv(Jan_12_ytm,Jan_13_ytm)
x_4<-rv(Jan_13_ytm,Jan_14_ytm)
x_5<-rv(Jan_14_ytm,Jan_17_ytm)
x_6<-rv(Jan_17_ytm,Jan_18_ytm)
x_7<-rv(Jan_18_ytm,Jan_19_ytm)
x_8<-rv(Jan_19_ytm,Jan_20_ytm)
x_9<-rv(Jan_20_ytm,Jan_21_ytm)
rv_ytm<-data.frame(x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9)
cov_matrix_ytm<-cov(rv_ytm)
cov_matrix_ytm=cov_matrix_ytm*10^5
d_cov_matrix_ytm<-as.data.frame(cov_matrix_ytm)
print(xtable(d_cov_matrix_ytm),math.style.exponents = TRUE,file ="cov_table_ytm.tex")
```


```{r,echo=FALSE}
random_variable_1<-c(1:4)
rv_f<-function(fc_1,fc_2){
  random_variable_1<-c(log(fc_2[1]/fc_1[1]))
  random_variable_1[2]=log(fc_2[2]/fc_1[2])
  random_variable_1[3]=log(fc_2[3]/fc_1[3])
  random_variable_1[4]=log(fc_2[4]/fc_1[4])
  return(random_variable_1)
}
```

```{r,echo=FALSE}
y_1<-rv_f(Jan_10_forward,Jan_11_forward)
y_2<-rv_f(Jan_11_forward,Jan_12_forward)
y_3<-rv_f(Jan_12_forward,Jan_13_forward)
y_4<-rv_f(Jan_13_forward,Jan_14_forward)
y_5<-rv_f(Jan_14_forward,Jan_17_forward)
y_6<-rv_f(Jan_17_forward,Jan_18_forward)
y_7<-rv_f(Jan_18_forward,Jan_19_forward)
y_8<-rv_f(Jan_19_forward,Jan_20_forward)
y_9<-rv_f(Jan_20_forward,Jan_21_forward)
y_1<-rv_f(Jan_21_forward,Jan_24_forward)
```

```{r,echo=FALSE}
rv_f<-data.frame(y_1,y_2,y_3,y_4,y_5,y_6,y_7,y_8,y_9)
cov_matrix_f<-cov(rv_f)
cov_matrix_f = cov_matrix_f * 10^3
d_cov_matrix_f <- as.data.frame(cov_matrix_f)
print(xtable(d_cov_matrix_f),math.style.exponents = TRUE,file ="cov_table_forward.tex")
```


```{r,echo=FALSE}
e_ytm <-eigen(d_cov_matrix_ytm)
e_f <- eigen(d_cov_matrix_f)

e_ytm$values
e_f$values

e_ytm_v<-as.data.frame(e_ytm$vectors)
e_f_v<-as.data.frame(e_f$vectors)

print(xtable(e_ytm_v),math.style.exponents = TRUE,file ="eigenvectors of ytm.tex")
print(xtable(d_cov_matrix_f),math.style.exponents = TRUE,file ="eigenvetors of f.tex")
```