library(readxl)
library(ggplot2)
library(dplyr)
l
file_path <- "C:/Users/miche/Downloads/Step 3 excel.xlsx"

excel_sheets(file_path)
df <- read_excel(file_path, sheet = 1)

head(df)
df <- read_excel(file_path, sheet = 1, skip = 1)
head(df) #df is for the P/S


ggplot(df, aes(x = Year, y = Revenue, color = Company)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Revenue Over Time: Reddit vs Meta vs Snap",
       y = "Revenue (in millions or billions)",
       x = "Year") +
  theme_minimal()


names(df)
library(tidyr)
library(dplyr)

df_long <- df %>%
  pivot_longer(cols = c(Reddit, Meta, Snap),
               names_to = "Company",
               values_to = "Revenue")


library(tidyr)
library(dplyr)

df_long <- df %>%
  pivot_longer(cols = c(Reddit, Meta, Snap),
               names_to = "Company",
               values_to = "Revenue")
library(ggplot2)
# y = Revenue,
ggplot(df_long, aes(x = Year, y = Revenue, color = Company,fill = Company)) +
  geom_bar(stat = "identity") +
  labs(title = "P/S : Reddit vs Meta vs Snap",
       y = "P/S ratio",
       x = "Year") +
  theme_minimal()

library(tidyr)library(tidyr)library(tidyr)
library(dplyr)

df_long <- df %>%
  pivot_longer(cols = c(Reddit, Meta, Snap),
               names_to = "Company",
               values_to = "Revenue")


head(df_long)

names(df_long)[1] <- "Year"
head(df_long)
names(df_long)
library(ggplot2)

ggplot(df_long, aes(x = Year, y = Revenue, color = Company)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(title = "P/S: Reddit vs Meta vs Snap",
       y = "P/S Ratio",
       x = "Year") +
  theme_minimal()





########## FOR THE NEXT SHEET
library(readxl)
df_other <- read_excel(file_path, sheet = 2) #OPERATING MARGIN
head(df_other)
names(df_other)[1] <- "Year"
library(tidyr)
df_other_long <- df_other %>%
  pivot_longer(cols = c(Reddit, Meta, Snap),
               names_to = "Company",
               values_to = "OperatingMargin")
head(df_other_long)
library(ggplot2)

ggplot(df_other_long, aes(x = Year, y = OperatingMargin, color = Company)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(title = "Operating Margin Over Time: Reddit vs Meta vs Snap",
       y = "Operating Margin (%)",
       x = "Year") +
  theme_minimal()
################################### SHEET 3
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)   # for parse_number()
library(ggplot2)

df_other2 <- read_excel(file_path, sheet = 3) %>%           # “Revenue” sheet
  rename(
    Year   = `Fiscal Year End`,
    Reddit = `Revenue...2`,
    Meta   = `Facebook Revenue`,
    Snap   = `Revenue...5`
  ) %>% 
  select(-`Snap`) %>%      
  relocate(Year)              

convert_to_numeric <- function(x) {
  multiplier <- case_when(
    str_detect(x, "[Bb]") ~ 1e9,
    str_detect(x, "[Mm]") ~ 1e6,
    TRUE                  ~ 1
  )
  parse_number(x) * multiplier
}

df_other2 <- df_other2 %>% 
  mutate(across(c(Reddit, Meta, Snap), convert_to_numeric))

df_other_long2 <- df_other2 %>% 
  pivot_longer(cols = -Year,
               names_to   = "Company",
               values_to  = "Revenue")

ggplot(df_other_long2, aes(x = Year, y = Revenue, color = Company)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = " B")) +
  labs(
    title = "Revenue Over Time: Reddit vs. Meta vs. Snap",
    x     = "Fiscal Year End",
    y     = "Revenue (billions USD)"
  ) +
  theme_minimal()

####################################
# ── libraries ──────────────────────────────────────────
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
opm <- tribble(
  ~Year, ~Reddit, ~Meta,  ~Snap,
  2024, -43.11,   42.41, -14.68,
  2023, -17.43,   37.21, -30.36,
  2022, -25.82,   28.78, -30.32,
  2021, -26.05,   39.65, -17.05,
  2020, -27.33,   38.01, -34.39
)

opm_long <- opm %>%
  pivot_longer(cols = c(Reddit, Meta, Snap),
               names_to  = "Company",
               values_to = "Margin")

ggplot(opm_long, aes(x = Year, y = Margin, color = Company)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Operating Margin Over Time (FY 2020 – 2024)",
       x = "Fiscal Year",
       y = "Operating Margin (%)",
       color = NULL) +
  theme_minimal()

ggplot(opm_long, aes(x = factor(Year), y = Margin, fill = Company)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Year‑by‑Year Operating Margin Comparison",
       x = "Fiscal Year",
       y = "Operating Margin (%)",
       fill = NULL) +
  theme_minimal()


###################################################
# ── libraries ──────────────────────────────────────────
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

growth <- tribble(
  ~Period,      ~Reddit, ~Facebook, ~Snap,
  "2021–2022",   37.5,    -1.1,      11.7,
  "2022–2023",   20.6,     15.7,      0.2,
  "2023–2024",   61.7,     21.9,     16.3
)

# Make sure Periods stay in chronological order
growth <- growth %>% mutate(Period = factor(Period, levels = Period))

growth_long <- growth %>% 
  pivot_longer(cols = c(Reddit, Facebook, Snap),
               names_to  = "Company",
               values_to = "Growth")

ggplot(growth_long, aes(x = Period, y = Growth, group = Company, color = Company)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Year by Year Revenue Growth",
    x     = "Fiscal‑Year Pair",
    y     = "Growth Rate (%)",
    color = NULL
  ) +
  theme_minimal()

ggplot(growth_long, aes(x = Period, y = Growth, fill = Company)) +
  geom_col(position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Revenue Growth Comparison by Period",
    x     = "Fiscal Year Pair",
    y     = "Growth Rate (%)",
    fill  = NULL
  ) +
  theme_minimal()


##########################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

df_roe <- tibble::tribble(
  ~Year, ~Reddit,  ~Meta, ~Snap,
  2024,  -27.12,   37.14, -28.69,
  2023,   -6.23,   28.04, -52.95,
  2022,  -10.42,   18.52, -44.88,
  2021,  -18.07,   31.10, -15.95
)


df_roe_long <- df_roe %>%
  pivot_longer(
    cols = -Year,
    names_to = "Company",
    values_to = "ROE"
  )

ggplot(df_roe_long, aes(x = Year, y = ROE, color = Company, group = Company)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Return on Equity (ROE) Over Time",
    x = "Fiscal Year",
    y = "ROE (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12)
  )

