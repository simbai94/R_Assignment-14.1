blogdata = read.csv('blogData_train.csv',header = FALSE)
test1 = read.csv('blogData_test-2012.02.01.00_00.csv',header = FALSE)
View(blogdata)
#boxplot(blogdata)



#w = blogdata[, which(colSums(blogdata) != 0)]
#v = Filter(function(x) median(x)  == 0, w)
#str(v)
#summary(v)

plot(x = blogdata$V2,y = blogdata$V7)

#
library("Hmisc")
sum(object = is.na(blogdata))
blogdata2 = na.exclude(blogdata)

mean(blogdata$V3,na.rm = TRUE)

res2 <- rcorr(as.matrix(blogdata))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res2<-rcorr(as.matrix(complete.cases(blogdata) ))
res2$r
res2$P
a <- flattenCorrMatrix(res2$r, res2$P)
colnames(a)
class()
b <- a[order(a$p),]
b

b <- a[order(abs(a$cor)),]
top_five <- tail(x = b,n = 10,na.rm = TRUE)

top_five
#

limit <- nrow(blogdata)*0.8
blogdata_train <- blogdata[, which(as.numeric(colSums(blogdata != 0)) > limit)]
View(blogdata_train)

str(blogdata_train)
summary(blogdata_train)

cor(blogdata_train)

as.table(cor(blogdata_train))

table(blogdata$V281)

library(car)
library(MASS)
library(caret)

model = glm(blogdata$V281 ~ V1 + V2 + V6 + V7 + V11 + V14 + V17 + V19 + V21 + V22 + V23 + V61, data = blogdata_train)
summary(object = model)
step = stepAIC(model,direction = 'both')



model2 = glm(V61 ~.,data = blogdata)

step = stepAIC(model2,direction = 'both')
summary(object = model2)


y = test1$V281
X = test1[,c('V1','V2','V6' ,'V7' , 'V11' , 'V14' , 'V17', 'V19' , 'V21', 'V22' , 'V23' , 'V61')]

pred_link = predict(object = model,newdata = X,type = 'response')#testing the model on the test data set

confusionMatrix(data = pred_link,reference = as.factor(test1$V281))
summary(pred_link)

mse = function(y_hat, y) {
  mse = mean((y - y_hat)^2)
  return(mse)
}


