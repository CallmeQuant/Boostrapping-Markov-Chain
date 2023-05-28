library(dplyr)
# Loading RData file 
load(file = "boostrap_dat.RData")


# Discrete Time Markov Chain 
observed_df <- data_final %>%
  filter(id == "HOBBIES_1_192_WI_2"  & dates < "2016-04-18") %>%
  mutate(State = case_when(sales == 0 ~ "Zero",
                           sales > 0 ~ "Positive")) %>%
  select(sales, State)

# Estimating the  Markov Chain 
state <- observed_df %>% pull(State) %>%
  as.character()

library(markovchain)
mc <- markovchainFit(state, method = "laplace")$estimate

# Examine the property of Markov Chain 
verifyMarkovProperty(state)
assessStationarity(state, 10)


# Predict 7 days ahead 
pred <- predict(object = mc, newdata = "Zero", n.ahead = 7)

# Extracting df with positive sales only
positive_df <- data_final %>%
  filter(id == "HOBBIES_1_192_WI_2" & dates < "2016-04-18") %>%
  filter(sales > 0)

# Creating a sample with historical positive sales 
sample_positive_sale <- positive_df %>% pull(sales) %>%
  unique()

# Retrieving last observation
lastobs <- data_final %>%
  filter(id == "HOBBIES_1_192_WI_2" & dates == "2016-04-17") %>% pull(sales)

lastobs <- if_else(lastobs == 0, "Zero", "Positive")

# Markov Chain Boostrapping Function
mc.sim <- function(trans_mat, num.iters = 1000, n_ahead = 7, method = c("modified","simple")){
  type = method[1]
  states <- numeric(n_ahead) # Not Including the last observed demand
  states[1] <- lastobs
  sample_positive_sale <- positive_df %>% pull(sales) %>%
    unique()
  result <- matrix(0, nrow = num.iters, ncol = n_ahead)
  for (i in 1:num.iters){
    fc_seq <- predict(object = trans_mat, newdata = lastobs, n.ahead = 7)
    # if (lastobs == "Positive"){
    #   fc_seq[i] <- ifelse(runif(1) < trans_mat[1], )
    # }
    for (j in 1:n_ahead){
      if (fc_seq[j] == "Zero") {fc_seq[j] = as.numeric(0)}
      if (fc_seq[j] == "Positive"){
        x = sample(sample_positive_sale, 1, replace= TRUE)
        if (type == "Simple"){
          fc_seq[j] = 1 + as.integer(x + sqrt(x) * rnorm(1))
          if (fc_seq[j] < 0) {fc_seq[j] = x}
          else {
            fc_seq[j] = as.integer(0.5 + x + sqrt(x) * rnorm(1))
            if (fc_seq[j] < 0) {fc_seq[j] = 1}
          }
        }
      }
    }
    result[i, ] <- fc_seq
  }
  result <- apply(result, 2, as.numeric)
  result_bs <- colMeans(result)
  df <- tibble(Forecast = result_bs)
  return(df)
}


# Given at 2016-04-17, we have a postive sales => Simulate for 7 day ahead

result <- mc.sim(mc, num.iters = 1000, n_ahead = 7, method = "modified") %>%
  bind_cols(data_final %>%
              filter(id == "HOBBIES_1_192_WI_2" & dates > "2016-04-17") %>% select(sales))


# Define accuracy metric for intermittent demand 
ZAPE <- function(actual, pred){
  loss <- 0
  y <- actual
  yhat <- pred
  for (i in 1:length(y)){
    if (y[i] == 0){
      loss <- loss + yhat[i]
    }
    else{
      loss <- loss + abs((y[i] - yhat[i])/y[i])
    }
  }
  return((loss / length(y)) * 100)
}

print(result)
ZAPE(result$sales, result$Forecast)