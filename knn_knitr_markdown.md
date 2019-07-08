## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
## Data loading
hr <- read.csv("HR.csv", na.string = c("", " "))
origin_name <- colnames(hr)
colnames(hr) <- abbreviate(colnames(hr)) # column 이름을 약자로 바꿔주는 함수
summary(hr) # 데이터 요약
```

```
##       lst_             nmb_            av__            tm__       
##  Min.   :0.3600   Min.   :2.000   Min.   : 96.0   Min.   : 2.000  
##  1st Qu.:0.5600   1st Qu.:3.000   1st Qu.:156.0   1st Qu.: 3.000  
##  Median :0.7200   Median :4.000   Median :200.0   Median : 3.000  
##  Mean   :0.7161   Mean   :3.803   Mean   :201.1   Mean   : 3.498  
##  3rd Qu.:0.8700   3rd Qu.:5.000   3rd Qu.:245.0   3rd Qu.: 4.000  
##  Max.   :1.0000   Max.   :7.000   Max.   :310.0   Max.   :10.000  
##  NA's   :15                                       NA's   :5       
##       Wrk_             p__5             slry           sts_       
##  Min.   :0.0000   Min.   :0.00000   high  :1237   Min.   :0.0900  
##  1st Qu.:0.0000   1st Qu.:0.00000   low   :7316   1st Qu.:0.4400  
##  Median :0.0000   Median :0.00000   medium:6446   Median :0.6400  
##  Mean   :0.1446   Mean   :0.02127                 Mean   :0.6128  
##  3rd Qu.:0.0000   3rd Qu.:0.00000                 3rd Qu.:0.8200  
##  Max.   :1.0000   Max.   :1.00000                 Max.   :1.0000  
##                                                                   
##       left       
##  Min.   :0.0000  
##  1st Qu.:0.0000  
##  Median :0.0000  
##  Mean   :0.2381  
##  3rd Qu.:0.0000  
##  Max.   :1.0000  
## 
```


## Preprocessing


```r
# NA 데이터 제거

library(DMwR)
hr <- hr[!is.na(hr$tm__), ] # tm__변수가 NA값을 가지는 데이터를 제거

# NA 값 대치
hr <- knnImputation(hr, k = 5)

# 중복 데이터 제거
dup_idx <- which(duplicated(hr)) # 중복데이터 인덱스 추출
length(dup_idx) # 3037개의 중복 데이터 존재
```

```
## [1] 3037
```

```r
hr_clean <- hr[!duplicated(hr), ]  # 중복 데이터 제거

# 수치형 데이터 추출
hr_num <- hr_clean[,sapply(hr_clean, is.numeric) ] # 수치형 데이터만 추출

# train / validation / test set
flag <- sample(c("tr", "va", "te"), size = nrow(hr_num), c(6, 2, 2), replace = T)
train <- hr_num[which(flag == "tr"), ]
valid <- hr_num[which(flag == "va"), ]
test <- hr_num[which(flag == "te"), ]

# standardization 
tr_x <- scale(train[, 1:7], center = T, scale = T)
va_x <- scale(valid[, 1:7], center = attr(tr_x, "scaled:center"), scale = attr(tr_x, "scaled:scale"))
te_x <- scale(test[, 1:7], center = attr(tr_x, "scaled:center"), scale = attr(tr_x, "scaled:scale"))

tr_y <- as.factor(train[, 8])
va_y <- as.factor(valid[, 8])
te_y <- as.factor(test[, 8])
```
## Knn algorithm

```r
library(class)

# find optimal k 
acc_k <- NULL # accuracy를 누적할 NULL 벡터 생성
for(i in 1:30){
  m_knn <- knn(tr_x, va_x, cl = tr_y, k = i) # k = 1부터 30까지 바꿔가며 knn algorithm 학습
  t <- table(va_y, m_knn) 
  acc <- sum(diag(t)) / sum(t)
  acc_k <- c(acc_k, acc) # k = i의 accuracy를 acc_k 벡터에 누적
}
plot(acc_k, type ="l")
```

![plot of chunk Knn algorithm](figure/Knn algorithm-1.png)

```r
which.max(acc_k) # 최대 accuracy를 나타내는 optimal k
```

```
## [1] 3
```

```r
# knn algorithm with optinal k
m_knn <- knn(tr_x, va_x, cl = tr_y, k = which.max(acc_k)) # k = 5일 때 validation set의 예측값 
m_knn[1:10]
```

```
##  [1] 1 1 1 1 1 1 1 1 1 1
## Levels: 0 1
```

```r
### training accuracy
t <- table(va_y, m_knn) # 실제 y 값과 예측된 y값의 confusion matrix 생성 
t
```

```
##     m_knn
## va_y    0    1
##    0 1969   23
##    1   25  334
```

```r
acc <- sum(diag(t)) / sum(t) # accuracy 계산
acc
```

```
## [1] 0.9795832
```

```r
### test accuracy
final_knn <- knn(tr_x, te_x, cl = tr_y, k = 1) # k = 1을 이용하여 test set 적용
result <- table(te_y, final_knn)
result
```

```
##     final_knn
## te_y    0    1
##    0 1950   50
##    1   52  379
```

```r
acc_3 <- sum(diag(result)) / sum(result)
acc_3 # 최종 accuracy
```

```
## [1] 0.958042
```


## Performance Evaluation{.tabset}

```r
### training accuracy
t <- table(va_y, m_knn) # 실제 y 값과 예측된 y값의 confusion matrix 생성 
t
```

```
##     m_knn
## va_y    0    1
##    0 1969   23
##    1   25  334
```

```r
acc <- sum(diag(t)) / sum(t) # accuracy 계산
acc
```

```
## [1] 0.9795832
```

```r
### test accuracy
final_knn <- knn(tr_x, te_x, cl = tr_y, k = 1) # k = 1을 이용하여 test set 적용
result <- table(te_y, final_knn)
result
```

```
##     final_knn
## te_y    0    1
##    0 1950   50
##    1   52  379
```

```r
acc_3 <- sum(diag(result)) / sum(result)
acc_3 # 최종 accuracy
```

```
## [1] 0.958042
```
## Including Plots

You can also embed plots, for example:

![plot of chunk pressure](figure/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
