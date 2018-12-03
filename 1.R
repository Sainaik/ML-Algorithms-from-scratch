# Data Set
x1=c(1,1,1,1,1)
x2=c(1156,2100,517,1028,3075)
x3=c(2,3,1,2,4)
y=c(10,20,50,45,30)  # Dependant variable

#intial values of theta and alpha
t=c(0.01,0.009016,0.03)
a=0.0000001

# to store the remaining elements 
h=c()
j=c()
theta=list(length(y))
teta=c()

for (i  in 1:length(y))
{
  #Hypothesis
  h[i]=t[1]*x1[i]+t[2]*x2[i]+t[3]*x3[i]
  
  #cost function
  j[i]=(h[i]-y[i])^2/(2*length(y))
  
  #updating theta values
  t[1]=t[1]-(a*(1/length(y))*((h[i]-y[i])*x1[i]))
  t[2]=t[2]-(a*(1/length(y))*((h[i]-y[i])*x2[i]))
  t[3]=t[3]-(a*(1/length(y))*((h[i]-y[i])*x3[i]))
   
  theta[[i]]=t
  teta[1]=t[1]
  teta[2]=t[2]
  teta[3]=t[3]
  
}

#printing the cost values
print(h)

print(j)
#printing final theta values
print(teta)

# Test Data 
test=c(1156,2)
testOut=1*teta[1]+test[1]*teta[2]+test[2]*teta[3]
print(testOut)


