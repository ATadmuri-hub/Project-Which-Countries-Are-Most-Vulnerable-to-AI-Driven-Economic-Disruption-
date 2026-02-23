# ==============================================================================
# MIDTERM PROJECT — FINAL CLEAN SCRIPT
# Research question:
#   (1) Which European countries are most vulnerable to AI-driven economic disruption?
#   (2) What factors predict AI exposure (proxy = automation_rate from Kaggle)?
#
# Data notes:
#   - Structural predictors (WDI + Eurostat): 2015-2023
#   - Kaggle AI automation panel: 2015-2025
#   - Oxford Insights AI Readiness: 2025 (cross-sectional, parsed from PDF)
#   - Overlap for models using all sources: 2015-2023
# ==============================================================================

# ========================= SECTION 0: SETUP ==================================
message(">>> SECTION 0: Setup")

pkgs <- c(
  "tidyverse", "janitor", "WDI", "eurostat", "countrycode",
  "FactoMineR", "factoextra", "NbClust", "cluster", "pheatmap", "mclust",
  "sf", "rnaturalearth", "rnaturalearthdata", "ggrepel", "readxl"
)
install.packages(setdiff(pkgs, installed.packages()[, "Package"]), quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# --- Helpers ---
fix_eurostat_geo <- function(df, geo_col = "geo") {
  df |>
    filter(!str_detect(.data[[geo_col]], "^(EU|EA)")) |>
    mutate(!!sym(geo_col) := recode(.data[[geo_col]],
                                     "EL" = "GR", "UK" = "GB"))
}

add_country_name <- function(df, geo_col = "geo") {
  df |>
    mutate(country = countrycode(.data[[geo_col]], "iso2c", "country.name", warn = FALSE))
}

z <- function(x) as.numeric(scale(x))

winsorize <- function(x, p = 0.01) {
  q <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# Europe scope: EU27 + EFTA + UK
eu_iso2 <- eurostat::eu_countries$code
eu_iso3 <- countrycode(eu_iso2, "iso2c", "iso3c", warn = FALSE)
eu_iso3 <- c(eu_iso3, "ISL", "NOR", "CHE", "GBR") |> unique() |> na.omit()

start_year <- 2015L
end_year   <- 2023L

# ========================= SECTION 1: KAGGLE DATA =============================
message(">>> SECTION 1: Kaggle AI workforce automation panel")

ai_auto <- read_csv(
  file.path(Sys.getenv("HOME"),
            "global_ai_workforce_automation_2015_2025.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  transmute(
    year             = as.integer(year),
    country          = country,
    iso3c            = countrycode(country, "country.name", "iso3c", warn = FALSE),
    automation_rate  = automation_rate_percent,
    ai_readiness_score = ai_readiness_score,
    ai_invest_bil    = ai_investment_billion_usd,
    reskill_invest_mil = reskilling_investment_million_usd,
    ai_policy_index  = ai_policy_index,
    job_displacement_m = job_displacement_million,
    job_creation_m   = job_creation_million,
    productivity_index = productivity_index,
    employment_rate  = employment_rate_percent,
    avg_salary_usd   = average_salary_usd
  ) |>
  filter(!is.na(iso3c), between(year, 2015L, 2025L)) |>
  filter(iso3c %in% eu_iso3)

cat("Kaggle panel:", nrow(ai_auto), "rows,",
    n_distinct(ai_auto$iso3c), "EU countries,",
    min(ai_auto$year), "-", max(ai_auto$year), "\n")

# ========================= SECTION 2: OXFORD AI READINESS =====================
message(">>> SECTION 2: Oxford Insights AI Readiness (parsed from 2025 PDF)")

ai_readiness <- read_csv("ai_readiness_index.csv", show_col_types = FALSE) |>
  select(iso3c, country,
         AI_Readiness     = overall_score,
         Policy_Capacity  = policy_capacity,
         AI_Infrastructure = ai_infrastructure,
         OI_Governance    = governance,
         Public_Sector    = public_sector_adoption,
         Dev_Diffusion    = development_diffusion,
         OI_Resilience    = resilience)

# ========================= SECTION 3: STRUCTURAL PREDICTORS ===================
message(">>> SECTION 3: Fetching structural predictors (WDI + Eurostat)")

# 3.1 World Bank (WDI)
robot <- WDI(indicator = c(Robot_Density = "TX.VAL.TECH.MF.ZS"),
             start = start_year, end = end_year) |>
  select(iso3c, country, year, Robot_Density)

rd <- WDI(indicator = c(RD_Intensity = "GB.XPD.RSDV.GD.ZS"),
           start = start_year, end = end_year) |>
  select(iso3c, country, year, RD_Intensity)

edu <- WDI(indicator = c(Tertiary_Edu = "SE.TER.CUAT.BA.ZS"),
            start = start_year, end = end_year) |>
  select(iso3c, country, year, Tertiary_Edu)

gdp <- WDI(indicator = c(GDP_pc = "NY.GDP.PCAP.KD"),
            start = start_year, end = end_year) |>
  select(iso3c, country, year, GDP_pc)

# 3.2 Eurostat
desi <- tryCatch({
  get_eurostat("isoc_sk_dskl_i21", time_format = "num") |>
    filter(indic_is == "I_DSK2_AB", ind_type == "IND_TOTAL", unit == "PC_IND") |>
    fix_eurostat_geo("geo") |>
    rename(iso2c = geo, year = TIME_PERIOD, DESI_Total = values) |>
    add_country_name("iso2c") |>
    mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) |>
    filter(!is.na(iso3c), year >= start_year, year <= end_year) |>
    select(iso3c, country, year, DESI_Total)
}, error = function(e) {
  message("  DESI proxy failed: ", e$message)
  tibble(iso3c = character(), country = character(), year = integer(), DESI_Total = numeric())
})

emp_skill <- tryCatch({
  get_eurostat("lfsa_egan22d", time_format = "num") |>
    filter(TIME_PERIOD >= start_year, TIME_PERIOD <= end_year,
           sex == "T", age == "Y15-74", unit == "THS_PER") |>
    mutate(skill = case_when(
      str_starts(nace_r2, "A|B|C") ~ "Low",
      str_starts(nace_r2, "M|N|O|P") ~ "High",
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(skill)) |>
    fix_eurostat_geo("geo") |>
    group_by(geo, TIME_PERIOD, skill) |>
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = skill, values_from = values, names_prefix = "Emp_") |>
    rename(iso2c = geo, year = TIME_PERIOD) |>
    mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) |>
    filter(!is.na(iso3c)) |>
    select(iso3c, year, Emp_Low, Emp_High)
}, error = function(e) {
  message("  Employment-by-skill failed: ", e$message)
  tibble(iso3c = character(), year = integer(), Emp_Low = numeric(), Emp_High = numeric())
})

hh_inet <- tryCatch({
  get_eurostat("isoc_ci_in_h", time_format = "num") |>
    filter(TIME_PERIOD >= start_year, hhtyp == "TOTAL", unit == "PC_HH") |>
    fix_eurostat_geo("geo") |>
    rename(iso2c = geo, year = TIME_PERIOD, HH_Internet = values) |>
    add_country_name("iso2c") |>
    mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) |>
    filter(!is.na(iso3c), year >= start_year, year <= end_year) |>
    select(iso3c, country, year, HH_Internet)
}, error = function(e) {
  message("  Household internet failed: ", e$message)
  tibble(iso3c = character(), country = character(), year = integer(), HH_Internet = numeric())
})

unemp <- tryCatch({
  get_eurostat("une_rt_a", time_format = "num") |>
    filter(TIME_PERIOD >= start_year, sex == "T", unit == "PC_ACT", age == "Y15-74") |>
    fix_eurostat_geo("geo") |>
    rename(iso2c = geo, year = TIME_PERIOD, Unemp_Total = values) |>
    add_country_name("iso2c") |>
    mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) |>
    filter(!is.na(iso3c), year >= start_year, year <= end_year) |>
    select(iso3c, country, year, Unemp_Total)
}, error = function(e) {
  message("  Unemployment failed: ", e$message)
  tibble(iso3c = character(), country = character(), year = integer(), Unemp_Total = numeric())
})

youth_unemp <- tryCatch({
  get_eurostat("une_rt_a", time_format = "num") |>
    filter(TIME_PERIOD >= start_year, sex == "T", unit == "PC_ACT", age == "Y15-24") |>
    fix_eurostat_geo("geo") |>
    rename(iso2c = geo, year = TIME_PERIOD, Unemp_Youth = values) |>
    add_country_name("iso2c") |>
    mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) |>
    filter(!is.na(iso3c), year >= start_year, year <= end_year) |>
    select(iso3c, country, year, Unemp_Youth)
}, error = function(e) {
  message("  Youth unemployment failed: ", e$message)
  tibble(iso3c = character(), country = character(), year = integer(), Unemp_Youth = numeric())
})

# ========================= SECTION 4: MERGE MASTER PANEL ======================
message(">>> SECTION 4: Merging master panel")

all_data <- rd |>
  full_join(edu,         by = c("iso3c", "year", "country")) |>
  full_join(gdp,         by = c("iso3c", "year", "country")) |>
  full_join(robot,       by = c("iso3c", "year", "country")) |>
  full_join(emp_skill,   by = c("iso3c", "year")) |>
  full_join(unemp,       by = c("iso3c", "year", "country")) |>
  full_join(youth_unemp, by = c("iso3c", "year", "country")) |>
  full_join(hh_inet,     by = c("iso3c", "year", "country")) |>
  full_join(desi,        by = c("iso3c", "year", "country")) |>
  left_join(ai_readiness, by = c("iso3c"), suffix = c("", ".oi")) |>
  left_join(ai_auto,      by = c("iso3c", "year"), suffix = c("", ".kg")) |>
  filter(iso3c %in% eu_iso3, !is.na(iso3c)) |>
  # Resolve duplicate country columns from joins
  mutate(country = coalesce(country, country.oi, country.kg)) |>
  select(-matches("^country\\.(oi|kg)$")) |>
  # Deduplicate: keep one row per iso3c-year (take mean of numerics)
  group_by(iso3c, country, year) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

cat("Master panel:", nrow(all_data), "rows,",
    n_distinct(all_data$iso3c), "countries\n")

# ========================= SECTION 5: DATA AUDIT ==============================
message(">>> SECTION 5: Data audit")

core_structural <- c("RD_Intensity", "Robot_Density", "Emp_Low", "Emp_High",
                      "Tertiary_Edu", "HH_Internet", "GDP_pc",
                      "Unemp_Total", "Unemp_Youth")
core_present <- intersect(core_structural, names(all_data))

audit_coverage <- all_data |>
  summarise(
    n_rows = n(), n_countries = n_distinct(iso3c),
    year_min = min(year, na.rm = TRUE), year_max = max(year, na.rm = TRUE),
    across(all_of(core_present), ~ sum(!is.na(.x)), .names = "nonmiss_{.col}"),
    nonmiss_automation = sum(!is.na(automation_rate))
  )
print(audit_coverage)

dup_keys <- all_data |> count(iso3c, year, name = "n") |> filter(n > 1)
message("Duplicate iso3c-year keys: ", nrow(dup_keys))

miss_tbl <- all_data |>
  summarise(across(all_of(core_present), ~ mean(is.na(.x)), .names = "miss_{.col}")) |>
  pivot_longer(everything(), names_to = "var", values_to = "pct_missing") |>
  mutate(var = str_remove(var, "^miss_")) |>
  arrange(desc(pct_missing))
print(miss_tbl)

# ========================= SECTION 6: COMPLETE-CASE PANEL =====================
message(">>> SECTION 6: Complete-case structural panel")

df_cc <- all_data |>
  filter(year >= start_year, year <= end_year) |>
  filter(!if_any(all_of(core_present), is.na)) |>
  select(iso3c, country, year, all_of(core_present), automation_rate)

# Convert employment counts to shares
df_cc <- df_cc |>
  mutate(
    Emp_Total      = Emp_Low + Emp_High,
    Emp_Low_share  = Emp_Low / Emp_Total,
    Emp_High_share = Emp_High / Emp_Total
  )

# Analysis variables: replace raw counts with shares, drop one (sums to 1)
analysis_vars <- c("RD_Intensity", "Robot_Density", "Tertiary_Edu", "HH_Internet",
                    "GDP_pc", "Unemp_Total", "Unemp_Youth", "Emp_Low_share")

message("Complete-case: ", nrow(df_cc), " rows, ",
        n_distinct(df_cc$iso3c), " countries, ",
        min(df_cc$year), "-", max(df_cc$year))
message("Analysis vars: ", paste(analysis_vars, collapse = ", "))

# ========================= SECTION 7: EDA =====================================
message(">>> SECTION 7: EDA (outliers + correlations)")

# Outlier counts (|z| > 3)
z_outliers <- df_cc |>
  select(all_of(analysis_vars)) |>
  mutate(across(everything(), z)) |>
  summarise(across(everything(), ~ sum(abs(.x) > 3, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_outliers") |>
  arrange(desc(n_outliers))
print(z_outliers)

# Correlation matrix
cor_mat <- cor(df_cc[, analysis_vars], use = "pairwise.complete.obs")
pheatmap(cor_mat, border_color = "white", display_numbers = TRUE,
         number_format = "%.2f", fontsize = 9,
         main = "Correlation Matrix (Structural Analysis Variables)")

# ========================= SECTION 8: PCA =====================================
message(">>> SECTION 8: PCA")

pca_res <- PCA(df_cc[, analysis_vars], scale.unit = TRUE, ncp = 5, graph = FALSE)

# Eigenvalues
eig_tbl <- get_eigenvalue(pca_res) |>
  as_tibble(rownames = "PC") |>
  mutate(PC = paste0("PC", row_number()))
print(eig_tbl)

fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 60), title = "Scree Plot")

# Loadings
load_tbl <- get_pca_var(pca_res)$coord |>
  as.data.frame() |> rownames_to_column("variable") |> as_tibble() |>
  select(variable, Dim.1, Dim.2) |> arrange(desc(abs(Dim.1)))
print(load_tbl)

# Contributions
contrib_tbl <- get_pca_var(pca_res)$contrib |>
  as.data.frame() |> rownames_to_column("variable") |> as_tibble() |>
  select(variable, Dim.1, Dim.2) |> arrange(desc(Dim.1))
print(contrib_tbl)

fviz_pca_var(pca_res, col.var = "cos2", repel = TRUE,
             title = "PCA Variable Map (cos2)")

# Attach PCA scores
scores <- as_tibble(pca_res$ind$coord[, 1:3]) |>
  rename(PC1 = Dim.1, PC2 = Dim.2, PC3 = Dim.3) |>
  mutate(row_id = row_number())

df_cc2 <- df_cc |>
  mutate(row_id = row_number()) |>
  left_join(scores, by = "row_id") |>
  select(-row_id)

# ========================= SECTION 9: VULNERABILITY INDEX =====================
message(">>> SECTION 9: Vulnerability index (Exposure - Resilience)")

df_cc2 <- df_cc2 |>
  mutate(
    ExposureScore   = z(Emp_Low_share) + z(Robot_Density),
    ResilienceScore = z(GDP_pc) + z(Tertiary_Edu) + z(RD_Intensity) +
                      z(HH_Internet) + z(Emp_High_share),
    VulnerabilityIndex = ExposureScore - ResilienceScore
  )

print(summary(df_cc2 |> select(ExposureScore, ResilienceScore, VulnerabilityIndex)))

# ========================= SECTION 10: CLUSTERING =============================
message(">>> SECTION 10: Clustering (k selection + stability)")

pca_space <- df_cc2 |> select(PC1, PC2, PC3)
k_grid <- 2:8

# Elbow (WSS)
wss_tbl <- purrr::map_dfr(k_grid, \(k) {
  set.seed(123)
  km <- kmeans(pca_space, centers = k, nstart = 50)
  tibble(k = k, tot_withinss = km$tot.withinss)
})

# Silhouette
sil_tbl <- purrr::map_dfr(k_grid, \(k) {
  set.seed(123)
  km <- kmeans(pca_space, centers = k, nstart = 50)
  ss <- silhouette(km$cluster, dist(pca_space))
  tibble(k = k, avg_silhouette = mean(ss[, "sil_width"]))
})

ggplot(wss_tbl, aes(k, tot_withinss)) + geom_line() + geom_point() +
  labs(title = "Elbow Plot", x = "k", y = "Total Within SS") + theme_minimal()


ggplot(sil_tbl, aes(k, avg_silhouette)) + geom_line() + geom_point() +
  labs(title = "Average Silhouette by k", x = "k", y = "Avg Silhouette") + theme_minimal()

# NbClust majority vote
set.seed(123)
nb <- NbClust(as.data.frame(pca_space), distance = "euclidean",
              min.nc = 2, max.nc = 8, method = "kmeans", index = "all")

k_votes <- as.numeric(nb$Best.nc[1, ])
k_votes <- k_votes[!is.na(k_votes) & k_votes >= 2 & k_votes <= 8]

vote_tbl <- tibble(k = k_votes) |> count(k, name = "votes") |> arrange(desc(votes))
print(vote_tbl)

k_nb  <- vote_tbl |> slice_max(votes, n = 1, with_ties = FALSE) |> pull(k)
k_sil <- sil_tbl  |> slice_max(avg_silhouette, n = 1, with_ties = FALSE) |> pull(k)
message("NbClust majority k = ", k_nb, "; silhouette-best k = ", k_sil)

k_final <- k_nb
message("Using k_final = ", k_final)

ggplot(vote_tbl, aes(factor(k), votes)) + geom_col(fill = "steelblue") +
  geom_text(aes(label = votes), vjust = -0.4) +
  labs(title = paste0("NbClust Majority Vote (selected k = ", k_final, ")"),
       x = "k", y = "Votes") + theme_minimal()

# Final k-means
set.seed(123)
km <- kmeans(pca_space, centers = k_final, nstart = 100)
df_cc2 <- df_cc2 |> mutate(cluster = factor(km$cluster))

fviz_cluster(km, data = as.data.frame(pca_space),
             ggtheme = theme_minimal(), main = "K-Means Clusters in PCA Space")

# Cluster profiles (z-scored)
cluster_profiles_z <- df_cc2 |>
  select(cluster, all_of(analysis_vars), Emp_High_share) |>
  mutate(across(-cluster, z)) |>
  group_by(cluster) |>
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
message("Cluster profiles (z-scores):")
print(cluster_profiles_z)

# Cluster profiles (raw means)
cluster_profiles_raw <- df_cc2 |>
  group_by(cluster) |>
  summarise(across(all_of(c(analysis_vars, "Emp_High_share")), \(x) mean(x, na.rm = TRUE)),
            n = n(), .groups = "drop")
message("Cluster profiles (raw):")
print(cluster_profiles_raw)

# Stability (ARI across seeds)
seeds <- c(1, 7, 42, 99, 202)
cl_mat <- purrr::map(seeds, \(s) { set.seed(s); kmeans(pca_space, k_final, nstart = 100)$cluster })

ari_tbl <- expand_grid(i = seq_along(seeds), j = seq_along(seeds)) |>
  filter(i < j) |>
  mutate(ARI = purrr::map2_dbl(i, j, \(a, b) adjustedRandIndex(cl_mat[[a]], cl_mat[[b]])),
         seed_i = seeds[i], seed_j = seeds[j]) |>
  select(seed_i, seed_j, ARI)
message("Cluster stability (ARI):")
print(ari_tbl)

# ========================= SECTION 11: VULNERABILITY RANKINGS =================
message(">>> SECTION 11: Country vulnerability rankings")

latest_struct_year <- max(df_cc2$year)

rank_vuln <- df_cc2 |>
  filter(year == latest_struct_year) |>
  group_by(iso3c, country) |>
  summarise(VulnerabilityIndex = mean(VulnerabilityIndex),
            ExposureScore = mean(ExposureScore),
            ResilienceScore = mean(ResilienceScore),
            automation_rate = mean(automation_rate, na.rm = TRUE),
            cluster = first(cluster), .groups = "drop") |>
  arrange(desc(VulnerabilityIndex))

message("Top 15 most vulnerable (year = ", latest_struct_year, "):")
print(head(rank_vuln, 15))
message("Bottom 15 least vulnerable:")
print(tail(rank_vuln, 15))

# ========================= SECTION 12: AI EXPOSURE (KAGGLE 2025) ==============
message(">>> SECTION 12: AI exposure ranking (Kaggle automation_rate, 2025)")

rank_auto_2025 <- ai_auto |>
  filter(year == 2025) |>
  group_by(iso3c, country) |>
  summarise(automation_rate = mean(automation_rate),
            ai_readiness_score = mean(ai_readiness_score), .groups = "drop") |>
  arrange(desc(automation_rate))

message("Top 15 by automation rate (2025):")
print(head(rank_auto_2025, 15))

# ========================= SECTION 13: PREDICTIVE MODEL =======================
message(">>> SECTION 13: Predict automation_rate from structural predictors (2015-2023)")

model_df <- df_cc2 |>
  select(iso3c, country, year, automation_rate, all_of(analysis_vars)) |>
  filter(!is.na(automation_rate))

message("Model sample: ", nrow(model_df), " rows, ",
        n_distinct(model_df$iso3c), " countries, ",
        min(model_df$year), "-", max(model_df$year))

# Pooled OLS (descriptive)
m1 <- lm(automation_rate ~ ., data = model_df |> select(-iso3c, -country, -year))
cat("\n--- Pooled OLS ---\n")
print(summary(m1))

# Two-way FE (country + year dummies)
m2 <- lm(automation_rate ~ . + factor(year),
         data = model_df |> select(-country) |> mutate(iso3c = factor(iso3c)))
cat("\n--- Two-way FE ---\n")
print(summary(m2))

# ========================= SECTION 14: MAP ====================================
message(">>> SECTION 14: Map (clusters, latest structural year)")

df_map <- df_cc2 |>
  filter(year == latest_struct_year) |>
  select(iso3c, cluster)

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(iso_a3, geometry)

map_data <- left_join(world, df_map, by = c("iso_a3" = "iso3c"))

ggplot(map_data) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.3) +
  scale_fill_viridis_d(option = "D", name = "Cluster", na.value = "grey90") +
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72)) +
  labs(title = "European Country Clusters (Structural PCA Space)",
       subtitle = paste0("Year: ", latest_struct_year,
                         " | K-means (k=", k_final, ") on PC1-PC3"),
       caption = "Data: WDI + Eurostat | Exposure: Kaggle | AI Readiness: Oxford Insights 2025") +
  theme_minimal() +
  theme(legend.position = "bottom")

message(">>> DONE. All outputs ready for report.")

