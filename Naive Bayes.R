# Training Data For Genders
height=c(6.0,5.92,5.58,5.92,5.0,5.5,5.42,5.75)
weight=c(180,190,170,165,100,150,130,150)
foot=c(12,11,12,10,6,8,7,9)
sex=c("male","male","male","male","female","female","female","female")
X=data.frame(height,weight,foot,sex)
#mean of Data
Mean=aggregate(X[,1:3],list(X$sex),mean)
#variance of Data
Var=aggregate(X[,1:3],list(X$sex),var)
print(Mean)
print(Var)
#test data
test=c(6.0,130.0,8.0)
female_h=exp(-(test[1]-Mean[[1,2]])^2/(2*Var[[1,2]]))/sqrt(2*(3.14)*Var[[2,2]])
female_w=exp(-(test[2]-Mean[[1,3]])^2/(2*Var[[1,3]]))/sqrt(2*(3.14)*Var[[2,3]])
female_f=exp(-(test[3]-Mean[[1,4]])^2/(2*Var[[1,4]]))/sqrt(2*(3.14)*Var[[2,4]])
male_h=exp(-(test[1]-Mean[[2,2]])^2/(2*Var[[2,2]]))/sqrt(2*(3.14)*Var[[2,2]])
male_w=exp(-(test[2]-Mean[[2,3]])^2/(2*Var[[2,3]]))/sqrt(2*(3.14)*Var[[2,3]])
male_f=exp(-(test[3]-Mean[[2,4]])^2/(2*Var[[2,4]]))/sqrt(2*(3.14)*Var[[2,4]])
female=female_f*female_w*female_h*0.5
male=male_h*male_w*male_f*0.5
print("Postertior of Female :")
print(female)
print("Postertior of male")
print(male)
if(male>female)
{
  print(" Given Data Belongs to Male")
}else{
  print("  Given Data Belongs to female")
}

