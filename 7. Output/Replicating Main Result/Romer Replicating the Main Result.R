##########################################
#Author: Carl Romer
#Date: 2025/11/11
##########################################

##########################################
#  Setup and Load Dependencies
##########################################

install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
}
install_and_load(
  c(
    'tidyverse',
    'haven',
  'patchwork'  # for combining plots (like grc1leg)
  )
)

setwd('/Users/carlromer/Documents/UCSD/Computations/')

##########################################
# Read data
##########################################

data <- read_dta("6. Replication package/data/ms_blel_jpal_long.dta")

##########################################
# Helper function for one subject plot
##########################################

make_fig2 <- function(df, score_var, subject_title, y_upper) {
  df %>%
    group_by(treat, round) %>%
    summarise(
      mean_score = mean(.data[[score_var]], na.rm = TRUE),
      se = sd(.data[[score_var]], na.rm = TRUE) / sqrt(n())
    ) %>%
    mutate(
      treat_label = ifelse(treat == 1, "Treatment", "Control"),
      lower = mean_score - 1.96 * se,
      upper = mean_score + 1.96 * se
    ) %>%
    ggplot(aes(x = factor(round), y = mean_score, fill = treat_label)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.7, color = "black") +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.9), width = 0.2) +
    scale_fill_manual(values = c("gray80", "#003366")) +  # gs12 and dknavy
    labs(title = subject_title, y = "Score", x = NULL, fill = NULL) +
    coord_cartesian(ylim = c(-0.1, y_upper)) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "white", color = "white")
    )
}

# Create each panel
fig2a <- make_fig2(data, "m_theta_mle", "Mathematics", 0.8)
fig2b <- make_fig2(data, "h_theta_mle", "Hindi", 0.6)

# Combine the two panels with shared legend (like grc1leg)
fig2 <- (fig2a + fig2b + plot_layout(guides = "collect")) &
  theme(legend.position = "bottom")

##########################################
#Export
##########################################

# Save output (equivalent to "gr export ... fig2.png")
ggsave("7. Output/Replicating Main Result/fig2.pdf", fig2)
