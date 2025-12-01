############################################################
# Mindspark Extension: Heterogeneity by Baseline Grade
# Data: 6. Replication package/data/ms_blel_jpal_long.dta
############################################################

############################################################
# Setup
############################################################

setwd('/Users/carlromer/Documents/UCSD/Computations')
install_and_load <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cloud.r-project.org")
    }
    library(p, character.only = TRUE)
  }
}

install_and_load(c("haven", "dplyr", "lmtest", "sandwich", "stargazer"))

############################################################
#Load data
############################################################

dat_long <- haven::read_dta("6. Replication package/data/ms_blel_jpal_long.dta")

# Keep main analysis sample (same as paper)
# Keep analysis sample only (same as paper)
dat_long <- dat_long %>% dplyr::filter(in_r2 == 1)

# Baseline rows: round == 1
baseline <- dat_long %>%
  dplyr::filter(round == 1) %>%
  dplyr::select(
    st_id,
    strata,
    treat,
    st_grade1,
    m_bl = m_theta_mle,
    h_bl = h_theta_mle
  )

# Endline rows: round == 2
endline <- dat_long %>%
  dplyr::filter(round == 2) %>%
  dplyr::select(
    st_id,
    m_el = m_theta_mle,
    h_el = h_theta_mle
  )

# Merge to one row per student
panel <- baseline %>%
  dplyr::inner_join(endline, by = "st_id")


# Drop any missing scores just in case
panel <- panel %>%
  dplyr::filter(
    !is.na(m_bl), !is.na(m_el),
    !is.na(h_bl), !is.na(h_el)
  )

############################################################
# Create ordered grade index
############################################################

panel_iv <- panel %>%
  dplyr::mutate(
    grade_idx = dplyr::case_when(
      st_grade1 < 7  ~ -1,
      st_grade1 == 7 ~  0,
      st_grade1 == 8 ~  1,
      st_grade1 == 9 ~  2,
      TRUE           ~ NA_real_
    )
  ) %>%
  dplyr::filter(!is.na(grade_idx))  # drop missing-grade students

############################################################
#Regressions with grade_idx heterogeneity
############################################################

# Math:
math_mod_g <- lm(
  m_el ~ m_bl + treat * grade_idx + factor(strata),
  data = panel_iv
)

# Hindi:
hindi_mod_g <- lm(
  h_el ~ h_bl + treat * grade_idx + factor(strata),
  data = panel_iv
)

vcov_math_g  <- sandwich::vcovHC(math_mod_g, type = "HC1")
vcov_hindi_g <- sandwich::vcovHC(hindi_mod_g, type = "HC1")

cat("\n=== Math with grade index heterogeneity ===\n")
print(lmtest::coeftest(math_mod_g, vcov. = vcov_math_g))

cat("\n=== Hindi with grade index heterogeneity ===\n")
print(lmtest::coeftest(hindi_mod_g, vcov. = vcov_hindi_g))

############################################################
# Clean table (hiding strata FE)
############################################################

stargazer::stargazer(
  math_mod_g, hindi_mod_g,
  type = "html",
  se = list(
    sqrt(diag(vcov_math_g)),
    sqrt(diag(vcov_hindi_g))
  ),
  column.labels  = c("Math", "Hindi"),
  dep.var.labels = "Endline score",
  covariate.labels = c(
    "Baseline score",
    "Treatment (voucher)",
    "Grade index (-1,0,1,2)",
    "Treatment × Grade index"
  ),
  omit = c(
    "factor\\(strata\\)",
    "treat:grade_idx"
    ),
  add.lines = list(
    c("Strata FE", "Yes", "Yes")
  ),
  title = "Effect of Mindspark by Ordered Grade Index",
  out = "8. Extension/extension_grade_index.html"
)

cat("\nTable saved to: 8. Extension/extension_grade_index.html\n")
