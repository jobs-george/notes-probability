# ..Biased Coins
# .Description
# Suppose you have one fair coin and one biased coin that lands Heads with 
# probability p > 0.5, e.g., 3/4. 
# You pick a coin at random and flip it three times.
# It lands Heads all three times. 
# Given this information, what is the probability you picked the fair coin?

# .Functions
# Functions are blocks of reusable code. 
# They are declared with the keyword "function".
# Parameter values can be defaulted in their declaration.

biased_coins <- function(nsims = 10^3, seed = 42, n_heads = 3, bias = 0.75) {

  ## calculates posterior probability of biased coin problem
  
  set.seed(seed)

  # simulate choosing a fair (or biased coin)
  coin_choice <- sample(c("fair", "biased"), nsims, replace = TRUE)

  # initialize n-head-count variables
  count_fair <- count_biased <- 0

  for (each_coin in coin_choice) {
    
    seed <- seed + 1
    
    set.seed(seed)
    
    if (each_coin == "fair") {
      
      if (sum(sample(c(0,1), n_heads, replace = T)) == n_heads) {
        
        count_fair <- count_fair + 1
        
      }
      
    } else {
      
      if (sum(sample(c(0,1), n_heads, replace = T, prob = c(1-bias, bias))) == n_heads) {
        
        count_biased <- count_biased + 1
        
      }    
      
    }
    
  }
  
  posterior <- count_fair / (count_fair + count_biased)
  
  return(posterior)
  
}
               
# Set non-default function parameter values
n_heads <- 3
bias <- 0.75

debugonce(biased_coins)
biased_coins(10^4, n_heads = n_heads, bias = bias)

#> 0.2271248

# Compare this to Bayes' calculation
bayes_posterior <- ((1/(2^n_heads)) * (1/2)) / ((1/2) * ((1/(2^n_heads)) + (bias^n_heads)))
bayes_posterior

#> 0.2285714