# Training Data
x1=c(1000,2000,500,3000,1500,3500,4500,5000,5500,6000)
x2=c(2,4,1,6,3,7,9,10,11,12)
y=c(24,48,12,72,36,84,108,120,132,144) # Actual Output of Training Data
# caluculating cost
cost <- function(X, y, theta) 
  {
   sum( (X %*% theta - y)^2 ) / (2*length(y))
}
alpha = 0.00000001            # learning rate
num_iters = 15       #  number of iterations
cost_history = rep(0,num_iters) 
theta_history = list(num_iters)
theta =  c(0,0,0) # Initial values of theta
X = cbind(1,x1,x2) 
 
 for (i in 1:num_iters) 
   {
    
    theta[1] = theta[1] - alpha * (1/length(y)) * sum(((X%*%theta)- y)*X[,1])
    theta[2]= theta[2] - alpha * (1/length(y)) * sum(((X%*%theta)- y)*X[,2])
    theta[3] = theta[3] - alpha * (1/length(y)) * sum(((X%*%theta)- y)*X[,3])
    cost_history[i] = cost(X, y, theta)
    theta_history[[i]] <- theta
 } 
# final Theta Values
 print(theta)
 plot(1:num_iters,cost_history,col="red")
 
 #test data
 te1=c(1500,2500) # Area 
 te2=c(2,3)  # No of Bedrooms
 y2=c()
 for (i in 1:length(te1))
 {
   y2[i]=theta[1]+theta[2]*te1[i]+theta[3]*te2[i]
 }
 
 print(y2)  #output for  Test Data
 