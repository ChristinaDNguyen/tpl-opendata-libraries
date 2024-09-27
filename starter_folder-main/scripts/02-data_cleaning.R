# Read in libraries for this workspace.
library(tidyverse)

##### 1. READING AND MAKING DATA USABLE ##### 

# Read in the data about Toronto Public Libraries' branches' and their number
# of annual visits.
raw_data <- read_csv("data/raw_data/raw_data.csv")
raw_data

# Rename the headings of each column in the tibble, to more useful headings.

cleaned_data <- raw_data %>%
  rename(
    RowID = `_id`,       # Rename '_ID' to 'RowID'
    Year = Year,      # Keep 'Year' as 'Year'
    BranchName = BranchCode, # Rename 'BranchCode' to 'BranchName'
    AnnualNumberofVisits = Visits   # Rename 'Visits' to 'AnnualNumberofVisits'
  ) 
cleaned_data 

# The row called "BranchName" actually contains abbreviations for each branch's 
# name. Fix this to contain the full name by reading in another dataset 
# which contains the abbreiviation alongisde the full name, and isolate
# those two columns only, in this new dataset. Then, use it as a reference
# to replace() BranchName with the real names. E.g., 'AB' becomes Albion; 'ACD'
# becomes Albert Campbell, etc. This reference dataset can be found at
# https://open.toronto.ca/dataset/library-branch-general-information/.
# Note that the reference dataset does list some 'branches' which are not 
# physical spaces or full libraries, such as the 'Virtual Library' (VIR),
# 'Automated Phone System' (PR), and 'Interloan' (IL). This does not
# affect our replace() in the cleaned_data dataset, though.

reference_dataset <- read.csv("data/raw_data/tpl-branch-general-information-2023.csv")
reference_dataset 
reference_dataset <- reference_dataset %>%
  select(BranchCode, BranchName)
reference_dataset

cleaned_data <- cleaned_data %>%
  left_join(reference_dataset, by = c("BranchName" = "BranchCode"))

cleaned_data # R created a new column and called it 'BranchName.y' since
# we already have a column called 'BranchName'. Now remove the extra column
# that contains the abbreviations that we do not need for the analysis.

cleaned_data <- cleaned_data %>% 
  mutate(BranchName = BranchName.y) %>% # Make BranchName contain the data from BranchName.y
  select(-BranchName.y) # Delete " # Get rid of BranchName.y column.
cleaned_data

##### 2. CREATING SUMMARY STATISTICS #####

## 2.1 TOTAL VISITS PER BRANCH ##

# To create summary statistics, first I calculate the total visits per 
# branch over all the years from 2012 - 2022. That is, how many visits did
# Albion have over the 10 years? etc.

total_visits_per_branch <- cleaned_data %>%
  group_by(BranchName) %>%
  summarise(TotalVisits = sum(AnnualNumberofVisits, na.rm = TRUE))
total_visits_per_branch # This outputs a two-column table, with name of the
# branch on the left, and the total number of visits it had over ten years
# on the right. Perfect. Now reorganize the table so it displays the branches 
# from most to least visited.

total_visits_per_branch <- cleaned_data %>%
  group_by(BranchName) %>%
  summarise(TotalVisits = sum(AnnualNumberofVisits, na.rm = TRUE)) %>%
  arrange(desc(TotalVisits))

total_visits_per_branch # This shows the top 10 most visited libraries
# over the ten years are Toronto Reference Library, North York Central Library,
# Fairview, Woodside Square, Bridlewood, Agincourt, Cedarbrae, Bloor/Gladstone, 
# Richview, and Northern District. There are 104 rows total in this table, which
# includes the hospital libraries and the bookmobile library. Some possible
# "messiness" in the data could be that some collections exist in a 
# larger library space, yet are counted in total_visits_per_branch (derived
# from cleaned_data and raw_data) as separate libraries. This is the case
# for Osborne Collection and Merril Collection, both physically
# situated in Lillian H. Smith branch, yet counted as a separate library
# branch in the data collected.

# Look at the bottom ten in this total_visits_per_branch by using tail()

tail(total_visits_per_branch, n = 10) # This shows the bottom 10 most visited
# libraries over the ten years are Taylor Memorial, Northern Elms, Davenport, 
# St. Clair/Silverthorn, Todmorden Room, Swansea Memorial, Bookmobile One, 
# Sunnybrook Hospital, Osborne Collection, and Merril Collection.

# Put the top 10 and bottom 10 branches into their own datasets to easily call
# on later, when doing the analysis in Quarto. 

top_10_visits_per_branch <- total_visits_per_branch %>%
  slice_head(n = 10) 
top_10_visits_per_branch

bottom_10_visits_per_branch <- total_visits_per_branch %>%
  slice_tail(n=10)
bottom_10_visits_per_branch

## 2.2 MEAN, MEDIAN, MODE AND STANDARD DEVIATION ##

# Analyzing the mean, median, mode, and standard deviation of each branch's 
# visits over a 10-year period will give us good insights into 
# each branch's performance and trends. Calculate the summary
# statistics into a new dataset called branch_summary_statistics

library(dplyr) #using dplyr to do this summary statistics, but am still
# missing a way to calculate mode. I will write that function myself
# so R can calculate mode for me.

get_mode <- function(x) {
  uniq_x <- unique(x) 
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Return the mode here
}

# NOW I can finally make the summary

branch_summary_statistics <- cleaned_data %>%
  group_by(BranchName) %>%
  summarise(
    MeanVisits = mean(AnnualNumberofVisits, na.rm = TRUE),
    MedianVisits = median(AnnualNumberofVisits, na.rm = TRUE),
    ModeVisits = get_mode(AnnualNumberofVisits),
    StdDevVisits = sd(AnnualNumberofVisits, na.rm = TRUE)
  )

branch_summary_statistics

#This data can tell us:
# --> mean: the average number of visits tells us how popular that branch is 
#     during those ten years. We can then compare the mean between all
#     104 branches which we have data for.
# --> median: if a branch has a median of, say, 150,000 visits, but the
#     mean is much higher, this could show that some years had exceptionally
#     higher visits, for that branch. Likewise the inverse applies.
# --> mode: can show us peak years, but perhaps not the most useful data here
# --> SD: high standard deviation indicates significant fluctuations in 
#     yearly visits, and v.v.. E.g. If one branch has a standard deviation of 
#     100,000 visits while another has 20,000, this means that the first branch 
#     experiences more variability in its annual visits, which could be due 
#     to some factors like seasonal events, changes in community engagement,
#     or external influences; library administrators and event planners use 
#     this type of information to allocate resources.

# Graph this summary statistics profile, including mean and median

mean_and_median_graph <- ggplot(branch_summary_statistics, aes(x = reorder(BranchName, -MeanVisits))) +
  geom_bar(aes(y = MeanVisits), stat = "identity", fill = "steelblue", alpha = 0.6) +
  geom_point(aes(y = MedianVisits), color = "orange", size = 3) +
  labs(title = "Mean, Median, and Mode Annual Visits per Library Branch",
       x = "Library Branch",
       y = "Number of Visits") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Number of Visits")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  geom_errorbar(aes(ymin = MeanVisits - StdDevVisits, ymax = MeanVisits + StdDevVisits), 
                width = 0.2, color = "red", position = position_dodge(0.9))
mean_and_median_graph
# Given the graph above, mean_and_median_graph, most library branches visually 
# have medians (orange dots) close to the mean (blue bars). Only two branches
# have means significantly higher than the median, i.e. the two leftmost 
# branches on the graph. The labels on the x-axis are difficult to read due to 
# the large number of library branches, so we check: in the dataset, those two 
# branches are Toronto Reference Library and and North York Central Library.
# That means that, for TRL and NYCL, some years had exceptionally higher
# visits.

# Graph this summary statistics profile, including mean with standard deviation 
# as error bars.

sd_graph <- ggplot(branch_summary_statistics, aes(x = reorder(BranchName, -MeanVisits), y = MeanVisits)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.6) +
  geom_errorbar(aes(ymin = MeanVisits - StdDevVisits, ymax = MeanVisits + StdDevVisits), 
                width = 0.2, color = "red") +
  labs(title = "Mean Annual Visits per Library Branch with Standard Deviation",
       x = "Library Branch",
       y = "Mean Annual Visits") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
sd_graph #Yes we can see that the leftmost two branches, as we noticed earlier
# in the mean/median comparison, shows a high amount of variation in the  
# number of annual visits (the red bar is very large for these two).

##### 3. EXPORTING DATA TO USE LATER IN QUARTO DOCUMENT ##### 

write_csv(cleaned_data, "data/analysis_data/cleaned_data")
write_csv(top_10_visits_per_branch, "data/analysis_data/top_10_visits_per_branch.csv")
write_csv(bottom_10_visits_per_branch, "data/analysis_data/bottom_10_visits_per_branch.csv")

