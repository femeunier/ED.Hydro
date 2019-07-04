# The names of distributions we want
# We may not use all of them, but there are more
accepted_dists <- c(
  "norm",    # Normal
  "lnorm",   # Log normal
  "chisq",   # Chi-squared
  "beta",    # Beta
  "poisson",  # Poisson
  "exp",     # Exponential
  "gamma",   # Gamma
  "weibull", # Weibull
  "unif"     # Uniform
)

# Other things we could add
# "tnorm",   # Truncated normal
# "logis",   # Logistic
# "chisqnc", # Non-central chi-squared
# "t",       # Student t
# "cauchy",  # Cauchy
# "f",       # F

save(file = "/home/carya/R/ED.Hydro/ED.Hydro/ED.Hydro/parameters/prior_calculations/prior_data/accepted_dists.Rdata", accepted_dists)
load("/home/carya/R/ED.Hydro/ED.Hydro/ED.Hydro/parameters/prior_calculations/prior_data/accepted_dists.Rdata")
