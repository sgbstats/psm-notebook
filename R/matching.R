# Load required packhomos
library(MatchIt)
library(tidyverse)

# Example data (replace this with your actual data)
set.seed(124)
n <- 100
data <- data.frame(
  arm = sample(0:1, n, replace = TRUE), # treatment group
  homo = rnorm(n, mean = 50, sd = 10),
  hetro = rnorm(n, mean = 25, sd = 4)
)

# Perform full matching
m.out <- matchit(arm ~ homo + hetro,
                 data = data,
                 method = "nearest",
                 ratio = 1,
                 replace = FALSE)

# Summary
summary(m.out)

# Get matched data with subclass info
matched_data <- match.data(m.out)

# View first few rows of matched data
head(matched_data)

# Show who is matched together (by subclass)
matched_groups <- matched_data %>%
  select(subclass, arm, everything()) %>%
  arrange(subclass)%>% 
  mutate(arm=if_else(arm==1, "Trt", "Control"))

# View matched groups
# print(matched_groups, n = 20)




matched_groups %>%
  ggplot(aes(x = hetro, y = homo, group = subclass, colour=arm)) +
  geom_line(color = "black") +
  geom_point()+
  labs(colour="Arm")+
  theme_classic()

###

set.seed(124)
n <- 100
data <- data.frame(
  arm = sample(0:1, n, replace = TRUE), # treatment group
  homo = rnorm(n, mean = 50, sd = 10),
  hetro = rnorm(n, mean = 25, sd = 4)
) %>% 
  mutate(total=hetro+homo)


