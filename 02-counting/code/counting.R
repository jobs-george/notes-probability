# ..Prerequisites
# .Sampling
# The "sample" command generates pseudo random numbers *without* replacement
n <- 10; k <- 5
sample(n, k)

# Set replace to "TRUE" for sampling *with* replacement
sample(n, k, replace = TRUE)

# To generate a random permutation, use sample(n, n) or
sample(n)

# R can also draw from non-numeric vectors. For example
sample(letters, 7)

# The sample command also allows us to specify probabilities, e.g., where
# 4 is more likely to be sampled, twice as much as 2,
# and 4 times as likely as 1.
sample(c(1, 2, 3, 4), 3, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.4))

# For sampling without replacement, the probabilities are redistributed by
# solving the equation
#
# k*0.2 + k*0.3 + k*0.4 = 1
#
# The new probabilities will be 'proportional' to the original.

# ..Birthday Matches
# .Description
# Imagine a room with k people. 
# Assume that each person’s Birthday is equally likely.  
# Calculate the probability that 2 or more people have the same birthday.

# .Simulation
# To simulate the birthday problem, 
# use the sample function to choose 23 random birthdays
b <- sample(1:365, k, replace = TRUE)
table(b)

# Then simulate n times with a set random seed
set.seed(42)
nsims <- 10^4
r <- replicate(nsims, max(table(sample(1:365, k, replace = TRUE))))
sum(r>=2) / nsims

#> 0.5072

# .Comments: 
# - randomly choose 23 numbers w/ replacement between 1 and 365 corresponding to 23 people's birthdays. 
# - tally the numbers, and check if there is at least 1 match (max >= 2)
# - do this 10k times, and count the proportion of matches
# - above half of simulations have at least 1 match

# .Calculation
# The probability of at least one birthday match in a group of 23 people
# can be calculated from a combinatorial analysis
k <- 23
p_match <- 1 - prod((365-k+1):365)/(365^k)
p_match

#> 0.5072972

# Better yet, R has a built in function
pbirthday(k)

#> 0.5072972

# Or the number of people to have a match with probability p
qbirthday(0.5)

# The function also allows triple, quadruple matches
pbirthday(23, coincident = 3)

#> 0.01441541

