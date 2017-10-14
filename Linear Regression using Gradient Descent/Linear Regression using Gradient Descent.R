

setwd("E:/AML - BUAN 6341")

# ===============================================================
# loading the dataset
bikeRentalDataset <-
  read.csv(file = "hour.csv", header = TRUE, sep = ",")

#removing variables
bikeRentalDataset$yr <- NULL
bikeRentalDataset$registered <- NULL
bikeRentalDataset$casual <- NULL
bikeRentalDataset$dteday <- NULL
bikeRentalDataset$instant <- NULL

# setting partition, creating train and test dataset
train_size <- floor(0.7 * nrow(bikeRentalDataset))
sampleData <-
  sample.int(n = nrow(bikeRentalDataset),
             size = train_size,
             replace = F)
trainDataset <- bikeRentalDataset[sampleData,]
testDataset <- bikeRentalDataset[-sampleData,]

#defining dummy variable
buildDummy <- function(Dataset) {
  Dataset$B1 <- ifelse(Dataset$season == 1, 1, 0)
  Dataset$B2 <- ifelse(Dataset$season == 2, 1, 0)
  Dataset$B3 <- ifelse(Dataset$season == 3, 1, 0)
  Dataset$B4 <- ifelse(Dataset$mnth == 1, 1, 0)
  Dataset$B5 <- ifelse(Dataset$mnth == 2, 1, 0)
  Dataset$B6 <- ifelse(Dataset$mnth == 3, 1, 0)
  Dataset$B7 <- ifelse(Dataset$mnth == 4, 1, 0)
  Dataset$B8 <- ifelse(Dataset$mnth == 5, 1, 0)
  Dataset$B9 <- ifelse(Dataset$mnth == 6, 1, 0)
  Dataset$B10 <- ifelse(Dataset$mnth == 7, 1, 0)
  Dataset$B11 <- ifelse(Dataset$mnth == 8, 1, 0)
  Dataset$B12 <- ifelse(Dataset$mnth == 9, 1, 0)
  Dataset$B13 <- ifelse(Dataset$mnth == 10, 1, 0)
  Dataset$B14 <- ifelse(Dataset$mnth == 11, 1, 0)
  Dataset$B15 <- ifelse(Dataset$hr == 0, 1, 0)
  Dataset$B16 <- ifelse(Dataset$hr == 1, 1, 0)
  Dataset$B17 <- ifelse(Dataset$hr == 2, 1, 0)
  Dataset$B18 <- ifelse(Dataset$hr == 3, 1, 0)
  Dataset$B19 <- ifelse(Dataset$hr == 4, 1, 0)
  Dataset$B20 <- ifelse(Dataset$hr == 5, 1, 0)
  Dataset$B21 <- ifelse(Dataset$hr == 6, 1, 0)
  Dataset$B22 <- ifelse(Dataset$hr == 7, 1, 0)
  Dataset$B23 <- ifelse(Dataset$hr == 8, 1, 0)
  Dataset$B24 <- ifelse(Dataset$hr == 9, 1, 0)
  Dataset$B25 <- ifelse(Dataset$hr == 10, 1, 0)
  Dataset$B26 <- ifelse(Dataset$hr == 11, 1, 0)
  Dataset$B27 <- ifelse(Dataset$hr == 12, 1, 0)
  Dataset$B28 <- ifelse(Dataset$hr == 13, 1, 0)
  Dataset$B29 <- ifelse(Dataset$hr == 14, 1, 0)
  Dataset$B30 <- ifelse(Dataset$hr == 15, 1, 0)
  Dataset$B31 <- ifelse(Dataset$hr == 16, 1, 0)
  Dataset$B32 <- ifelse(Dataset$hr == 17, 1, 0)
  Dataset$B33 <- ifelse(Dataset$hr == 18, 1, 0)
  Dataset$B34 <- ifelse(Dataset$hr == 19, 1, 0)
  Dataset$B35 <- ifelse(Dataset$hr == 20, 1, 0)
  Dataset$B36 <- ifelse(Dataset$hr == 21, 1, 0)
  Dataset$B37 <- ifelse(Dataset$hr == 22, 1, 0)
  Dataset$B38 <- ifelse(Dataset$holiday == 1, 1, 0)
  Dataset$B39 <- ifelse(Dataset$weekday == 0, 1, 0)
  Dataset$B40 <- ifelse(Dataset$weekday == 1, 1, 0)
  Dataset$B41 <- ifelse(Dataset$weekday == 2, 1, 0)
  Dataset$B42 <- ifelse(Dataset$weekday == 3, 1, 0)
  Dataset$B43 <- ifelse(Dataset$weekday == 4, 1, 0)
  Dataset$B44 <- ifelse(Dataset$weekday == 5, 1, 0)
  Dataset$B45 <- ifelse(Dataset$workingday == 1, 1, 0)
  Dataset$B46 <- ifelse(Dataset$weathersit == 1, 1, 0)
  Dataset$B47 <- ifelse(Dataset$weathersit == 2, 1, 0)
  Dataset$B48 <- ifelse(Dataset$weathersit == 3, 1, 0)
  x <-
    as.matrix(Dataset[c(
      "B1",
      "B2",
      "B3",
      "B4",
      "B5",
      "B6",
      "B7",
      "B8",
      "B9",
      "B10",
      "B11",
      "B12",
      "B13",
      "B14",
      "B15",
      "B16",
      "B17",
      "B18",
      "B19",
      "B20",
      "B21",
      "B22",
      "B23",
      "B24",
      "B25",
      "B26",
      "B27",
      "B28",
      "B29",
      "B30",
      "B31",
      "B32",
      "B33",
      "B34",
      "B35",
      "B36",
      "B37",
      "B38",
      "B39",
      "B40",
      "B41",
      "B42",
      "B43",
      "B44",
      "B45",
      "B46",
      "B47",
      "B48",
      "temp",
      "atemp",
      "hum",
      "windspeed"
    )])
  #View(X)
  X <- cbind(rep(1, length(x[, 1])), x) # binding 1 for X0 term
  # vector of actual values
  y <- as.matrix(Dataset[c("cnt")])
  #write.csv(data.frame(cbind(X, y)), file = "traindata.csv")
  return(list(X, y))
}

buildDummyExpt4 <- function(Dataset) {
  Dataset$B4 <- ifelse(Dataset$mnth == 1, 1, 0)
  Dataset$B5 <- ifelse(Dataset$mnth == 2, 1, 0)
  Dataset$B6 <- ifelse(Dataset$mnth == 3, 1, 0)
  Dataset$B7 <- ifelse(Dataset$mnth == 4, 1, 0)
  Dataset$B8 <- ifelse(Dataset$mnth == 5, 1, 0)
  Dataset$B9 <- ifelse(Dataset$mnth == 6, 1, 0)
  Dataset$B10 <- ifelse(Dataset$mnth == 7, 1, 0)
  Dataset$B11 <- ifelse(Dataset$mnth == 8, 1, 0)
  Dataset$B12 <- ifelse(Dataset$mnth == 9, 1, 0)
  Dataset$B13 <- ifelse(Dataset$mnth == 10, 1, 0)
  Dataset$B14 <- ifelse(Dataset$mnth == 11, 1, 0)
  #Dataset$B15 <- ifelse(Dataset$holiday == 1, 1, 0)
  Dataset$B16 <- ifelse(Dataset$hr == 0, 1, 0)
  Dataset$B17 <- ifelse(Dataset$hr == 1, 1, 0)
  Dataset$B18 <- ifelse(Dataset$hr == 2, 1, 0)
  Dataset$B19 <- ifelse(Dataset$hr == 3, 1, 0)
  Dataset$B20 <- ifelse(Dataset$hr == 4, 1, 0)
  Dataset$B21 <- ifelse(Dataset$hr == 5, 1, 0)
  Dataset$B22 <- ifelse(Dataset$hr == 6, 1, 0)
  Dataset$B23 <- ifelse(Dataset$hr == 7, 1, 0)
  Dataset$B24 <- ifelse(Dataset$hr == 8, 1, 0)
  Dataset$B25 <- ifelse(Dataset$hr == 9, 1, 0)
  Dataset$B26 <- ifelse(Dataset$hr == 10, 1, 0)
  Dataset$B27 <- ifelse(Dataset$hr == 11, 1, 0)
  Dataset$B28 <- ifelse(Dataset$hr == 12, 1, 0)
  Dataset$B29 <- ifelse(Dataset$hr == 13, 1, 0)
  Dataset$B30 <- ifelse(Dataset$hr == 14, 1, 0)
  Dataset$B31 <- ifelse(Dataset$hr == 15, 1, 0)
  Dataset$B32 <- ifelse(Dataset$hr == 16, 1, 0)
  Dataset$B33 <- ifelse(Dataset$hr == 17, 1, 0)
  Dataset$B34 <- ifelse(Dataset$hr == 18, 1, 0)
  Dataset$B35 <- ifelse(Dataset$hr == 19, 1, 0)
  Dataset$B36 <- ifelse(Dataset$hr == 20, 1, 0)
  Dataset$B37 <- ifelse(Dataset$hr == 21, 1, 0)
  Dataset$B38 <- ifelse(Dataset$hr == 22, 1, 0)
  
  x <-
    as.matrix(Dataset[c(
      "B4",
      "B5",
      "B6",
      "B7",
      "B8",
      "B9",
      "B10",
      "B11",
      "B12",
      "B13",
      "B14",
      "B16",
      "B17",
      "B18",
      "B19",
      "B20",
      "B21",
      "B22",
      "B23",
      "B24",
      "B25",
      "B26",
      "B27",
      "B28",
      "B29",
      "B30",
      "B31",
      "B32",
      "B33",
      "B34",
      "B35",
      "B36",
      "B37",
      "B38",
      "temp"
    )])
  
  X <- cbind(rep(1, length(x[, 1])), x) # binding 1 for X0 term
  # vector of actual values
  y <- as.matrix(Dataset[c("cnt")])
  return(list(X, y))
}

#cost function
cost <- function(X, y, theta) {
  cost_result <- sum((X %*% theta - y) ^ 2) / (2 * length(y))
  return(cost_result)
}

# random number of iterations to initialize cost_history and theta_history list
iterations <-   8000
i <- 1
# calculate gradient descent for given theta, alpha, and Threshold
calGradientDescent <- function(X, y, theta, alpha, Threshold) {
  # capture the value of cost function after every iterations
  cost_history <- rep(0, iterations)
  # capture the value of theta after every iterations
  theta_history <-  list(iterations)
  
  deltaCost <- 0
  
  repeat {
    #update theta
    error <- (X %*% theta - y)
    theta <- theta - alpha * (1 / length(y)) * (t(X) %*% error)
    #calculate cost and store it
    cost_history[i] <- cost(X, y, theta)
    #View(cost_history[i])
    # capture updated theta value
    #print(cost_history[i])
    theta_history[[i]] <- theta
    if (i > 1) {
      deltaCost <-
        (cost_history[i - 1] - cost_history[i]) / cost_history[i - 1]
      if (deltaCost < Threshold) {
        # case when model converge, so break the loop
        #print("*******")
        #print(deltaCost)
        #remove extra rows
        #print(theta)
        cost_history <- cost_history[-(i + 1:iterations)]
        theta_history <- theta_history[-(i + 1:iterations)]
        iterations <- i
        results <-
          list(theta_history, cost_history, iterations, error)
        #View(i)
        #View(theta_history[[i]])
        #update iteration value to actual iterations
        
        return(results)
      }
    }
    i <- i + 1
    
  }
}

plot_function <- function(dt1, main_name, xlable, ylable) {
  plot(
    dt1,
    type = 'line',
    col = 'blue',
    main = main_name,
    ylab = ylable,
    xlab = xlable
  )
}

# randomly setting the value
# 48 categorical variable, 4 continuous variable and 1 intercept
theta <- matrix(c(0, 0), nrow = 53)#matrix(rnorm(54))
#print(theta)
alpha <- 0.01
Threshold <- 0.001

# for train dataset
outdataTrain <- buildDummy(trainDataset)
# 1 is X and 2 is y
X <- outdataTrain[[1]]
y <- outdataTrain[[2]]

outcome <- calGradientDescent(X, y, theta, alpha, Threshold)
theta_history <- outcome[[1]]
cost_history <- outcome[[2]]
num_iteration <- outcome[[3]]
Error_train <- outcome[[4]]

# theta_history[[length(theta_history)]] should give last theta values
Ynew_train <-
  as.matrix(ceiling((X %*% theta_history[[length(theta_history)]])))

# for test dataset
outdataTest <- buildDummy(testDataset)
Xtest <- (outdataTest[[1]])
ytest <- (outdataTest[[2]])
Ynew_test <-
  as.matrix(ceiling((Xtest %*% theta_history[[length(theta_history)]])))
Error_test <- Ynew_test - ytest

#plot_cost_function(cost_history)

#Experiment 1
Threshold <- 0.001 #large threshold value
alpha <- 0.01
exp1_cost_tr <- rep(0, 5)
exp1_alpha <- list(0.001, 0.009, 0.01, 0.05, 0.07) #rep(0,5)
exp1_cost_ts <- rep(0, 5)
for (m in 1:5) {
  #set new threshold
  alpha <- exp1_alpha[[m]]
  #For train
  outcome_tr_1 <- calGradientDescent(X, y, theta, alpha, Threshold)
  exp1_cost_tr[m] <- outcome_tr_1[[2]][length(outcome_tr_1[[2]])]
  #For Test
  outcome_ts_1 <-
    calGradientDescent(Xtest, ytest, theta, alpha, Threshold)
  exp1_cost_ts[m] <- outcome_ts_1[[2]][length(outcome_ts_1[[2]])]
}

plot(
  x = exp1_alpha,
  y = exp1_cost_tr,
  main = "Cost Vs learning",
  xlab = 'alpha',
  ylab = 'Cost',
  type = 'o',
  col = 'blue',
  lwd = 2
)
lines(exp1_alpha, exp1_cost_ts, type = 'o', col = "green")

alpha <- exp1_alpha[[1]]
outcome_exp1_1 <- calGradientDescent(X, y, theta, alpha, Threshold)
theta_hist_exp1 <- outcome_exp1_1[[1]]
Ynew_tr_exp1 <-
  as.matrix(ceiling((X %*% theta_hist_exp1[[length(theta_hist_exp1)]])))
Error_tr_exp1 <- Ynew_tr_exp1 - y
Ynew_test_exp1 <-
  as.matrix(ceiling((Xtest %*% theta_hist_exp1[[length(theta_hist_exp1)]])))
Error_test_exp1 <- Ynew_test_exp1 - ytest

plot_function(outcome_exp1_1[[2]], "Cost function", "interations", "cost")

alpha <- exp1_alpha[[5]]
outcome_exp1_2 <- calGradientDescent(X, y, theta, alpha, Threshold)
theta_hist_exp1 <- outcome_exp1_2[[1]]
Ynew_tr_exp1 <-
  as.matrix(ceiling((X %*% theta_hist_exp1[[length(theta_hist_exp1)]])))
Error_tr_exp1 <- Ynew_tr_exp1 - y
Ynew_test_exp1 <-
  as.matrix(ceiling((Xtest %*% theta_hist_exp1[[length(theta_hist_exp1)]])))
Error_test_exp1 <- Ynew_test_exp1 - ytest

plot_function(outcome_exp1_2[[2]], "Cost function", "interations", "cost")

#Experiment 2
Threshold <- 0.1 #large threshold value
alpha <- 0.01
exp2_cost_tr <- rep(0, 5)
exp2_thrhld <- rep(0, 5)
exp2_cost_ts <- rep(0, 5)
for (j in 1:5) {
  #set new threshold
  Threshold <- Threshold / 10
  exp2_thrhld[j] <- Threshold
  #For train
  outcome_tr <- calGradientDescent(X, y, theta, alpha, Threshold)
  exp2_cost_tr[j] <- outcome_tr[[2]][length(outcome_tr[[2]])]
  #For Test
  outcome_ts <-
    calGradientDescent(Xtest, ytest, theta, alpha, Threshold)
  exp2_cost_ts[j] <- outcome_ts[[2]][length(outcome_ts[[2]])]
}


plot(
  x = exp2_thrhld,
  y = exp2_cost_tr,
  main = "Cost Vs Threshold",
  xlab = 'Threshold',
  ylab = 'Cost',
  type = 'o',
  col = 'blue',
  lwd = 2
)
lines(exp2_thrhld, exp2_cost_ts,   type = 'o', col = "green")

#graph of cost for lowest threshold value
plot_function(outcome_tr[[2]],
              "Cost Vs iterations for Threshold 0.00001",
              "interations",
              "cost")

Threshold <- exp2_thrhld[4]
outcome_tr <- calGradientDescent(X, y, theta, alpha, Threshold)
#graph of cost for one less than smaller threshold value
plot(
  outcome_tr[[2]],
  main = "Cost Vs Iterations",
  ylab = "Cost",
  xlab = "no of iterations",
  type = 'l',
  col = 'blue'
)

# Experiment 3
# selecting hum, windspeed, temp as a random features
experiment3 <- function(theta_3) {
  x_tr_exp3 <- as.matrix(trainDataset[c("hum", "windspeed", "temp")])
  X_tr_exp3 <- cbind(c(rep(1, length(x_tr_exp3[, 1]))), x_tr_exp3)
  outcome_exp3 <-
    calGradientDescent(X_tr_exp3, y, theta_3, alpha, Threshold)
  theta_history_exp3 <- outcome_exp3[[1]]
  #View(theta_history[[1]])
  cost_history_exp3 <- outcome_exp3[[2]]
  num_iteration_exp3 <- outcome_exp3[[3]]
  # theta_history[[length(theta_history)]] should give last theta values
  Ynew_train_exp3 <-
    as.matrix(ceiling((X_tr_exp3 %*% theta_history_exp3[[length(theta_history_exp3)]])))
  Error_train_exp3 <- Ynew_train_exp3 - y
  # for test dataset of experiment 3
  x_ts_exp3 <- as.matrix(testDataset[c("hum", "windspeed", "temp")])
  X_ts_exp3 <- cbind(rep(1, length(x_ts_exp3[, 1])), x_ts_exp3)
  Ynew_test_exp3 <-
    as.matrix(ceiling((X_ts_exp3 %*% theta_history_exp3[[length(theta_history_exp3)]])))
  Error_test_exp3 <- Ynew_test_exp3 - ytest
  #plot(theta_history_exp3, main = "cost function",xlab = "no of iterations", ylab = "cost")
  exp3_out <-
    list(Error_train_exp3,
         Error_test_exp3,
         Ynew_train_exp3,
         Ynew_test_exp3)
  
  return(exp3_out)
}

#invoke experiment 3
theta_3 <-  matrix(c(0, 0), nrow = 4) # matrix(rnorm(4))
experiment3_output <- experiment3(theta_3)
exp_train3_error <- experiment3_output[[1]]
exp_test3_error  <- experiment3_output[[2]]
exp_train3_model <- experiment3_output[[3]]
exp_test3_model  <- experiment3_output[[4]]
plot(exp_train3_error ,
     main = "Error for train data",
     ylab = 'Error',
     type = 'p')

plot(exp_test3_error ,
     main = "Error for test data",
     ylab = 'Error',
     type = 'p')
# for All features
plot(Error_train ,
     main = "Error for train data for all features",
     ylab = 'Error',
     type = 'p')

plot(Error_test ,
     main = "Error for test data for all features",
     ylab = 'Error',
     type = 'p')

# Experiment 4
# selecting hr, mnth, temp for experiment 4
experiment4 <- function(theta_4) {
  data_tr_exp4 <- buildDummyExpt4(trainDataset)
  X_tr_exp4 <- data_tr_exp4[[1]]
  y_tr_exp4 <- data_tr_exp4[[2]]
  
  outcome_exp4 <-
    calGradientDescent(X_tr_exp4, y_tr_exp4, theta_4, alpha, Threshold)
  theta_history_exp4 <- outcome_exp4[[1]]
  cost_history_exp4 <- outcome_exp4[[2]]
  num_iteration_exp4 <- outcome_exp4[[3]]
  #View(theta_history)
  #theta_history_exp4[[length(theta_history_exp4)]] should give last theta values
  Ynew_train_exp4 <-
    as.matrix(ceiling((X_tr_exp4 %*% theta_history_exp4[[length(theta_history_exp4)]])))
  
  Error_train_exp4 <- Ynew_train_exp4 - y_tr_exp4
  
  # for test dataset of experiment 4
  data_ts_exp4 <- buildDummyExpt4(testDataset)
  X_ts_exp4 <- data_ts_exp4[[1]]
  y_ts_exp4 <- data_ts_exp4[[2]]
  
  Ynew_test_exp4 <-
    as.matrix(ceiling((X_ts_exp4 %*% theta_history_exp4[[length(theta_history_exp4)]])))
  
  Error_test_exp4 <- Ynew_test_exp4 - y_ts_exp4
  #plot(cost_history_exp4, main = "cost function",xlab = "no of iterations", ylab = "cost")
  exp4_out <-
    list(Error_train_exp4,
         Error_test_exp4,
         Ynew_train_exp4,
         Ynew_test_exp4)
  return(exp4_out)
}

#invoke experiment 4
theta_4 <-  matrix(c(0, 0), nrow = 36)
experiment4_output <- experiment4(theta_4)
exp_train4_error  <- experiment4_output[[1]]
exp_test4_error   <- experiment4_output[[2]]
exp_train4_model  <- experiment4_output[[3]]
exp_test4_model   <- experiment4_output[[4]]

plot(exp_train4_error ,
     main = "Error for train data",
     ylab = 'Error',
     type = 'p')

plot(exp_test4_error ,
     main = "Error for test data",
     ylab = 'Error',
     type = 'p')

#Based on experimentations generate model with new alpha and threshold 
alpha <- 0.01
Threshold <- 0.00001
X <- outdataTrain[[1]]
y <- outdataTrain[[2]]

Xtest <- (outdataTest[[1]])
ytest <- (outdataTest[[2]])

theta <- matrix(c(0, 0), nrow = 53)
outcome <- calGradientDescent(X, y, theta, alpha, Threshold)
theta_history <- outcome[[1]]
cost_history <- outcome[[2]]
num_iteration <- outcome[[3]]
Error_train <- outcome[[4]]
Ypredicted_train <- as.matrix(ceiling((X %*% theta_history[[length(theta_history)]])))
Ypredicted_test <- as.matrix(ceiling((Xtest %*% theta_history[[length(theta_history)]])))
#final_data <- data.frame(cbind(X,y))
#lm is used only to plot regrssion model output
#mdl <- lm(final_data$cnt~.,data = final_data)
#plot(mdl)
