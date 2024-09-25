

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("data/raw_data/raw_data.csv")
raw_data

# This doesn't need much cleaning other than renaming the headings

cleaned_data <- raw_data %>%
  rename(
    ID = `_id`,       # Rename `_id` to ID
    Year = Year,      
    BranchName = BranchCode, # Rename BranchCode to BranchName
    NumberofVisits = Visits   
  ) #easiest clean ever!

cleaned_data

# Calculate total visits per branch over all the years from 2012 - 2022
top_branches <- cleaned_data %>%
  group_by(BranchName) %>%
  summarize(TotalVisits = sum(NumberofVisits)) %>%
  arrange(desc(TotalVisits)) %>%
  slice_head(n = 10)  # Get top 10 branches, i.e. branches with largest total number of visits over all the years

top_branches

# Filter data to only include the top branches
top_data <- cleaned_data %>%
  filter(BranchName %in% top_branches$BranchName)
top_data

write_csv(cleaned_data, "data/analysis_data/analysis_data.csv")
write_csv(top_branches, "data/analysis_data/top_branches.csv")
write_csv(top_data, "data/analysis_data/top_data.csv")

