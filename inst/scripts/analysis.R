library(tidyverse)
library(spsurvey)
library(here)

# read in NWCA data
nwca <- read_csv(here("inst", "data", "nwca_2016.csv"))

# # add a national estimate variable
# nwca <- nwca |>
#   mutate(NATIONAL = "National")

# estimate nitrogen condition
ntl_cond_miss <- cat_analysis(
  dframe = nwca,
  vars = "NTL_COND",
  subpops = c("WETCLS_GRP", "NATIONAL"),
  weight = "WGT_TP",
  xcoord = "XCOORD",
  ycoord = "YCOORD"
)

ntl_cond_miss <- ntl_cond_miss |>
  filter(Category != "Total") |>
  mutate(Category = factor(Category, levels = c("Good", "Fair", "Poor", "Not Assessed"))) |>
  mutate(Subpopulation = factor(Subpopulation, levels = c("National", "EH", "EW", "PRLH", "PRLW")))



ntl_plot_miss <- ggplot(ntl_cond_miss, aes(x = Category, y = Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P), color = "black", linewidth = 0.75) +
  facet_grid(~ Subpopulation) +
  labs(y = "Percent", x = "NTL Condition Class") +
  scale_fill_viridis_d(option = "D", begin = 0.3) +
  theme(
    legend.position = "top",
    axis.text.x = element_blank()
  )


nwca <- nwca |>
  mutate(SW_SAMPLEABLE = replace_na(SW_SAMPLEABLE, "M")) |>
  mutate(CHEM_NOT_COLLECTED = replace_na(CHEM_NOT_COLLECTED, "M")) |>
  mutate(MICX_NOT_COLLECTED = replace_na(MICX_NOT_COLLECTED, "M")) |>
  mutate(WCHL_NOT_COLLECTED = replace_na(WCHL_NOT_COLLECTED, "M")) |>
  mutate(CMW_NOT_COLLECTED = str_c(CHEM_NOT_COLLECTED, MICX_NOT_COLLECTED,
                                    WCHL_NOT_COLLECTED)) |>
  mutate(WATER_PRESENT = case_when(
    CMW_NOT_COLLECTED == "YYY" ~ "No",
    CMW_NOT_COLLECTED == "MMY" ~ "Yes",
    CMW_NOT_COLLECTED == "MMM" ~ "Yes")
  )


h20_cond_miss <- cat_analysis(
  dframe = nwca,
  vars = "WATER_PRESENT",
  subpops = c("WETCLS_GRP", "NATIONAL"),
  weight = "WGT_TP",
  xcoord = "XCOORD",
  ycoord = "YCOORD"
)

h20_cond_miss <- h20_cond_miss |>
  filter(Category != "Total") |>
  mutate(Category = factor(Category, levels = c("Yes", "No"))) |>
  mutate(Subpopulation = factor(Subpopulation, levels = c("National", "EH", "EW", "PRLH", "PRLW")))

h20_plot <- ggplot(h20_cond_miss, aes(x = Category, y = Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P), color = "black", linewidth = 0.75) +
  facet_grid(~ Subpopulation) +
  labs(y = "Percent", x = "Water Present") +
  scale_fill_viridis_d(option = "D", begin = 0.3) +
  theme(
    legend.position = "top",
    axis.text.x = element_blank()
  )


ntl_cond_nomiss <- cat_analysis(
  dframe = nwca |> filter(WATER_PRESENT == "Yes"),
  vars = "NTL_COND",
  subpops = c("WETCLS_GRP", "NATIONAL"),
  weight = "WGT_TP",
  xcoord = "XCOORD",
  ycoord = "YCOORD"
)

ntl_cond_nomiss <- ntl_cond_nomiss |>
  filter(Category != "Total") |>
  mutate(Category = factor(Category, levels = c("Good", "Fair", "Poor", "Not Assessed"))) |>
  mutate(Subpopulation = factor(Subpopulation, levels = c("National", "EH", "EW", "PRLH", "PRLW")))

ntl_plot_nomiss <- ggplot(ntl_cond_nomiss, aes(x = Category, y = Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P), color = "black", linewidth = 0.75) +
  facet_grid(~ Subpopulation) +
  labs(y = "Percent", x = "NTL Condition Class") +
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
  theme(
    legend.position = "top",
    axis.text.x = element_blank()
  )
