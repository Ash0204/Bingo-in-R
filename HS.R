# Defining the parameters of the game, total numbers = n
# no. of players = k, size of each ticket = r, no. of times we play the game = phi
# install.packages("ggplot2")
library(ggplot2)

# Taking User inputs
n <- as.numeric(readline(prompt = "Enter the size of the main list of numbers : "))
k <- as.numeric(readline(prompt = "Enter the number of player lists in the game : "))
r <- as.numeric(readline(prompt = "Enter the size of the ticket : "))
phi <- as.numeric(readline("How many times you want to play the game : "))

# Defining and initializing required vectors
winning_list <- numeric(phi)
last_call_vec <- numeric(phi)

# Playing the game
for (m in seq(1,phi,1))
{
  # Creating the primary comparison list
  if(r%%2==0 && is.integer(n/2)==TRUE) {
    prime_list <- c(seq((n-r)/2,n/2-1,1),seq(n/2+1,(n+r)/2,1))
  } else if(r%%2!=0 && is.integer(n/2)==TRUE) {
    z <- (r-1)/2
    prime_list <- seq((n/2)-z,(n/2)+z,1)
  } else if(r%%2==0 && is.integer(n/2)==FALSE) {
    z <- round(n/2) - 1
    prime_list <- seq(z+1-r/2,z+r/2-1,1)
  } else if(r%%2!=0 && is.integer(n/2)==FALSE) {
    z <- round(n/2) - 1
    z1 <- round(r/2) - 1
    prime_list <- seq(z-z1,z+z1,1) # taking the lower value
  }
  
# Initializing the mean vector for checking the distinctness of each list of numbers
test <- FALSE
mean_vec <- numeric(k)
var_vec <- numeric(k)
name_vec <- vector(mode="character",length=k)
list_main <- seq(1,n,1)
while (test==FALSE)
  {
for (i in seq(1,k,1))
{
  assign(paste("plist",as.character(i),sep="_"),sample(list_main, r,replace=FALSE))
  mean_vec[i] <- mean(eval(parse(text = paste("plist",as.character(i),sep="_"))))
  var_vec[i] <- (sd(eval(parse(text = paste("plist",as.character(i),sep="_")))))^2
  name_vec[i] <- paste("plist",as.character(i),sep="_")
}
  test <- (length(unique(mean_vec))==k) && (length(unique(var_vec))==k)
}

# Update rules for respective lists
len_vec <- rep(r,k+1)
name_vec <- c(name_vec,"prime_list")
counter<-0
while(min(len_vec)!=0)
{
  con <- sample(list_main,1,rep=FALSE) # number called out from the main list
  for (i in seq(1,k+1,1))
  {
    assign(name_vec[i],eval(as.name(name_vec[i]))[!eval(as.name(name_vec[i])) %in% con])
    len_vec[i] <- length(eval(as.name(name_vec[i])))
  }
  list_main <- list_main[!list_main %in% con]
  counter <- counter + 1
}
last_call_vec[m] <- counter
winning_list[m] <- which.min(len_vec)
}
title <- "Checking normality for the last call vector"
s <- sd(last_call_vec)
xbar <- mean(last_call_vec)

# Trying to fit some distributions
ggplot(data.frame(last_call_vec), aes(sample=last_call_vec)) +
  stat_qq() +
  geom_abline(slope = s, intercept = xbar) +
  ggtitle(title)

title1 <- "Distribution of last_call_vec"
ggplot(data.frame(last_call_vec), aes(x=last_call_vec)) + 
  geom_histogram(aes(y=..density..),bins = sqrt(length(last_call_vec))+2,
                 fill = "grey",col="black") +
  geom_density(col = "red", lwd = 1) +
  stat_function(fun=dnorm,  args=list(mean=xbar, sd=s), col="blue", 
                lwd = 1) +
  ggtitle(title1) +
  xlab("Data") +
  ylab("Proportion") 

# As the distribution is skewed to the left taking log transformations
l1 <- log(last_call_vec)
title2 <- "Distribution of log(last_call_vec)"
xbar_log <- mean(l1)
sd_log <- sd(l1)
ggplot(data.frame(l1), aes(x=l1)) + 
  geom_histogram(aes(y=..density..),bins = sqrt(length(l1))+2,
                 fill = "grey",col="black") +
  geom_density(col = "red", lwd = 1) +
  stat_function(fun=dnorm,  args=list(mean=xbar_log, sd=sd_log), col="blue", 
                lwd = 1) +
  ggtitle(title2) +
  xlab("Data") +
  ylab("Proportion") 