# Graphs of simulated data
# Arseniy Khvorov
# Created 2019/11/22
# Last edit 2019/11/22

library(tidyverse)
library(ggdark) # devtools::install_github("khvorov45/ggdark")

# Directories to be used lated
data_dir <- "data"
graph_data_dir <- "graph-data"

# Functions ===================================================================

# Graph linear data
plot_lin <- function(dat) {
  dat %>%
    pivot_longer(c(x1, x2), names_to = "cov", values_to = "val") %>%
    ggplot(aes(y, val)) +
    dark_theme_bw(verbose = FALSE) +
    theme(
      strip.background = element_rect(fill = NA),
      axis.title.y = element_blank(),
      panel.spacing = unit(0, "null")
    ) +
    facet_grid(cov ~ filename, scales = "free_y") +
    geom_point(shape = 18)
}

# Read one dataframe
read_one <- function(filepath) {
  read_csv(filepath, col_types = cols()) %>%
    mutate(filename = str_replace(basename(filepath), ".csv", ""))
}

save_plot <- function(pl, name, folder) {
  ggsave_dark(
    file.path(folder, paste0(name, ".pdf")), pl, dark = TRUE,
    width = 15, height = 7.5, units = "cm", device = "pdf"
  )
}

# Script ======================================================================

all_files <- tools::list_files_with_exts(data_dir, "csv")
all_data <- map_dfr(all_files, read_one)

lin_data <- filter(all_data, str_detect(filename, "^lin"))
lin_plot <- plot_lin(lin_data)
save_plot(lin_plot, "lin", graph_data_dir)
