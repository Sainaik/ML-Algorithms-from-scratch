x1=c(6,5.92,5.58,5.92,5,5.5,5.42,5.75)
x2=c(180,190,170,165,100,150,130,150)
x3=c(12,11,12,10,6,8,7,9)
y=c(1,1,1,1,0,0,0,0)

X<-as.matrix(cbind(1,x1,x2,x3))
print(X)
Y<-as.matrix(y)
training<-function(data,label,alpha,iterations)
{
  m<-nrow(data)
  theta<-rep(0,ncol(data))
  cost_his<-c()
  sigmoid<-function(z)
  {
    g<-1/(1+exp(-z))
    return(g)
  }
  cost<-function(theta,X,Y)
  {
    g<-sigmoid(X%*%theta)
    j<-(1/m)*sum((-Y*log(g))-((1-Y)*log(1-g)))
    return(j)
  }
  for(i in 1:iterations)
  {
    theta<-theta-(alpha*(1/m))*(t(X)%*%(data%*%theta-Y))
    cost_his[i]<-cost(theta,data,Y)
    print(cost_his)
  }
  return(theta)
}
out=training(X,Y,0.0000008,10)

# for training Data 
test<-c(6,170,12)
T<-as.matrix(test)
hy<-(out[1,]*T[1,])+(out[2,]*T[2,])+(out[3,]*T[3,])
gtest<-1/(1+exp(-hy))

if(gtest>0.510)
{  
  print("Male")
}else{
  print("Female")
}