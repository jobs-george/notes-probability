# ..Introduction
# .Scripts
# R Code is executed line-by-line with Ctrl + Enter
print("hello world")

# Full scripts can be run with Ctrl + Shift + Enter
stopifnot(FALSE)

# .Vectors
# R is built around vectors. (The most useful statistical object).
# Use the 'c' (combine/concatenate) command to create a vector in R.
v <- c(3,1,4,1,5,9)

# Numbers are also (atomic) vectors. (Tip: Alt + "-" will generate 
# the assignment notation, thanks to Mark Liddell for this Tip.)
n <- 110

# There are many built-in functions, e.g., sum(), that adds all of the 
# values in a vector. (Also: max(), min(), and length().) 
sum(v)

# A shortcut for creating a vector 1 to n is
w <- 1:n

# We can also define
m <- 10
n <- 20

# so that
m:n

# We can access the ith entry in a vector with v[i],
# where v[1] is the first entry
v[c(1,3,5)]

# We can also create a sub-vector by deleting entries. 
# Note, the brackets are needed because -2:4 generates -2, -1, 0,...
v[-(2:4)]

# Operations are interpreted component-wise, 
# for example the following command generates (1/1, 1/2^2,...)
1/(1:100)^2

# In mathematics, adding vectors v + w of different lengths is 
# undefined. In R, recycling is used.
v + w

