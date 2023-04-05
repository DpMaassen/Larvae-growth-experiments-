# this is a template for the survival graphs for the larvae feeding experiments 
# the excelsheet containing the lengths should be formatted in the folling way: 
#the first collum contains de Day (age of the larvae in days post hatching) at which the lenght was measured. Collum name: "Day"
#collum 2 to collumn n  have the names of the treatments, please nu that for R to read it no spaces can be used. example name: Basic_Diet 

# now follows the Template code, all that is named "Fill' has to be given a name

# Reading the Excel file into a dataframe
# The Excel file has to be saved in the directory folder of the R project
dataframe_Fill <- read_excel("Fill.xlsx", sheet = 2) #sheet is in which tab of the excel file the lengts are stored

# Make a dataframe that is suitable for the analysis of the length
# Use pivot_longer with respect to "day" to calculate the mean length of larvae for each treatment per day 
dataframe_Fill2 <- dataframe_Fill %>% 
  pivot_longer(!c("Day")) %>%
  group_by(Day, name) %>%
  summarize(percentage = (value / 500) * 100)

# Make the ggplot graph with manually set scales and colors
FillSurvivalGraph <- dataframe_Fill2 %>%
  ggplot(aes(x = Day, y = percentage, color = as.factor(name), group = name)) +
  geom_line(position = position_dodge(width = 0.6), size = 1.5) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_hline(yintercept = 25, size = 1, linetype = "dotted") +
  geom_hline(yintercept = 0, size = 1, linetype = "dotted") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(from = 0, to = 100, by = 10)) +
  scale_x_continuous(limits = c(10, 42), breaks = seq(from = 10, to = 42, by = 5)) +
  scale_color_brewer(palette = "Set2")+ 
  ggtitle("Survival") +
  xlab("Day") + 
  ylab("Survival (%)") + 
  labs(color = "Treatment", subtitle = "First Phospholipids") + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 15, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 13)
  )
