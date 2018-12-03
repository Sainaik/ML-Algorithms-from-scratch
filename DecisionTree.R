path="C:\\Users\\sai kumar naik\\Desktop\\ML NIT\\diabetes.csv"
file=read.csv(path)
View(file)
set.seed(123)
id=sample(2,nrow(file),prob = c(0.7,0.3),replace = TRUE)
train=file[id==1,]
test=file[id==2,]
library("rpart")
colnames(file)
model=rpart(Outcome~.,data = train)


plot(model,margin = 0.1,col="green")
text(model,pretty = TRUE,cex=.8,use.n = TRUE, col="blue")
pred=predict(model,newdata = test)
for(i in   1:length(pred))
{
  if(pred[i]>0.5){
    pred[i]=1
  }
  else{
    pred[i]=0
  }
}
print(pred)
CM=table(pred,test$Outcome)

print(CM)
