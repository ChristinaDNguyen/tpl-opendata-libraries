# I have chosen to use the Open Toronto dataset about each TPL branch's 
# number of visits per year.

# First I load a package 
library(tidyverse)

# Then I simulate data

set.seed(555)

# Define the branches' names (not using all of them in this simulation; but 
#I will use them all in the final data. There's 100 branches in total!)

tpl_branches <- c("Reference Library", "Bloor/Gladstone", "Danforth/Coxwell", 
              "Parkdale", "Scarborough Civic Centre", "Yorkville", 
              "North York Central", "Spadina Road", "Fairview", "Albion")

# Create annual number of visits using Rohan's favorite distribution, Poisson
# Poisson distribution is good for simulating data in this case because 
# it's a probability distribution often used to model the number of events
# that occur within a fixed interval of time (here, that's ONE YEAR), 
# assuming these events happen independently and with a constant average
# rate (lamda)

annual_visits <- rpois(n = length(tpl_branches), lambda = 100000) #I make the 
#assumptiong that each branch generally gets about 100,000 visits per year
#on average

library_visits <- tibble(
  Branch = tpl_branches,
  Annual_visits = annual_visits
)

library_visits # That prints out tibble out in the console.

# Now make a graph of this simulated data

library_visits %>%
  ggplot(aes(x = Branch, y = Annual_visits)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Simulated Annual Visits per TPL Branch", 
       x = "Library Branch", 
       y = "Number of Visits") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
