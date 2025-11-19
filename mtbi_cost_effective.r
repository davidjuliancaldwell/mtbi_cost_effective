library(tidyverse)
library(readxl)
library(ggplot2)
library(ggbeeswarm)
library(janitor)
library(here)
library(ggrain)

########

# user defined inputs 
#boolean variables 
save_fig <- FALSE
save_fig_tables = TRUE
save_data <- TRUE
load_data <- FALSE 


# put the data sheet under data nested under the working directory
# and this script in the working directory
root_dir <- here()
data_dir <- root_dir
data_sheet <- 'RawDataForFigures_edit.xlsx'

data_fig_2 <- read_excel(file.path(data_dir,data_sheet),
  sheet = "Figure2"
)

data_fig_3 <- read_excel(file.path(data_dir,data_sheet),
  sheet = "Figure3"
)
## clean names

# using janitor clean_names function 
data_fig_2 <- data_fig_2 %>% janitor::clean_names()
data_fig_3 <- data_fig_3 %>% janitor::clean_names()

data_fig_2 <-   data_fig_2 %>%
  mutate(time= factor(
    time,
    levels = (c("before 2000","after 2000")),
  ))

data_fig_3 <- data_fig_3 %>%
  mutate(
    time = as.character(time) %>%
      str_replace_all("\u00A0", " ") %>%   # replace non-breaking space
      str_replace_all("[–—]", "-") %>%     # normalize dash types
      str_squish() %>%                     # trim and collapse spaces
      # optionally normalize case: tolower() 
      factor(levels = c("Acute hospitalization","1 yr post-injury","2 yrs post-injury","3 yrs post-injury"),
      ))

data_fig_3 <-   data_fig_3 %>%
  mutate(
    group= as.character(group) %>%
      str_replace_all("\u00A0", " ") %>%   # replace non-breaking space
      str_replace_all("[–—]", "-") %>%     # normalize dash types
      str_squish() %>%                     # trim and collapse spaces
      # optionally normalize case: tolower()
      factor(levels = (c("civilian","military"))
  ))

data_fig_3 <- data_fig_3 %>%
mutate(
  study = as.factor(study)
)

### figure 2

fig_2 <- ggplot(data_fig_2, aes(x = time, y = costs, color = group,size = n)) +
      geom_beeswarm(dodge.width = 0.5) + # Use dodge.width to avoid overlap
  labs(
    title = "Costs/Charges Reported for mTBI Stratified by Year ",
    subtitle = "Each point is one study",
    x = "Year of Study",
    y = "Amount (2025 USD Dollars)",
    color = "Costs/Charges",
    size = "Number of Subjects"
  ) +
      theme_minimal() +
    scale_color_hue(h = c(180, 300))

### figure 3

dodge_position <- position_dodge(width = 0.5)

 fig_3 <-  ggplot(data_fig_3, aes(x = time, y = amount, color = group)) +
      geom_beeswarm(aes(size = n)) + # Use dodge.width to avoid overlap
      geom_line(aes(group = study),alpha = 0.5) + # Connect points for each ID
      facet_wrap(vars(group),nrow=1) +
  labs(
    title = "Costs/Charges Reported for mTBI Stratified by Year ",
    subtitle = "Each point is one study",
    x = "Year of Study",
    y = "Amount (2025 USD Dollars)",
    color = "Costs/Charges",
    size = "Number of Subjects"
  )  +
      theme_minimal()


fig_2
fig_3

########

# raincloud plots

### figure 2

  fig_2_rain <- ggplot(data_fig_2, aes(x = time, y = costs, fill = group)) +
    geom_rain(alpha = 0.5, point.args = list(alpha = 0)) +         # stat layer — don't map size here
    geom_beeswarm(aes(size = n,color = group), dodge.width = 0.1, alpha = 0.7) +
    theme_classic() +
    scale_fill_hue(h = c(180, 300)) +
    scale_color_hue(h = c(180, 300)) +
    labs( 
      title = "Costs/Charges Reported for mTBI Stratified by Year ",
      subtitle = "Each point is one study",
      x = "Year of Study",
      y = "Amount (2025 USD Dollars)",
      size = "Number of Subjects"
    )
  ### figure 3

dodge_position <- position_dodge(width = 0.5)

 fig_3_rain <-  ggplot(data_fig_3, aes(x = time, y = amount, fill = group)) +
      geom_rain(alpha=0.5,point.args=list(drop=FALSE)) + # Use dodge.width to avoid overlap
  labs(
    title = "Costs/Charges Reported for mTBI Stratified by Year ",
    subtitle = "Each point is one study",
    x = "Year of Study",
    y = "Amount (2025 USD Dollars)",
    color = "Costs/Charges",
    size = "Number of Subjects"
  )  +
      theme_classic()


fig_2_rain
fig_3_rain