## Quilting Expense Analysis 2021 ##
# Date: 9/8/2021

# ------------------------------------------------------------------------------

library(tidyverse)
data <- read.csv("Quilting_Expenses_2.csv")

#-------------------------------------------------------------------------------

# Fix annoying double i created when importing data
expenses <- data %>% 
  select(ï..date, description, total, notes) %>% 
  rename(date = ï..date)

# Organize descriptions into categories, rename, establish y/n for if items were purchased for CCQAL
purchase_locations <- expenses %>% 
  group_by(description) %>% 
  mutate(notes = ifelse(notes == "Candy Crush QAL", "yes", "no")) %>% 
  rename(purchase_locations = description)

# Via histogram #
ggplot(data = purchase_locations, mapping = aes(x = total, y = purchase_locations)) +
  geom_bar(stat = "identity") +
  ggtitle("2021 Quilting Expense Analysis")

# Smaller category names, revert back to original graph
ggplot(data = purchase_locations, mapping = aes(x = purchase_locations, y = total)) +
  geom_bar(stat = "identity") +
  ggtitle("2021 Quilting Expense Analysis")

# Get total spending amounts for each purchase location
total_per_location <- purchase_locations %>%
  filter(purchase_locations, total) %>%
  mutate(purchase_locations, total_location = purchase_locations + total)

# Error - non-numeric argument to binary operator
# Confirm classes are character vs numeric incompatible
class(purchase_locations$purchase_locations)
class(purchase_locations$total)
