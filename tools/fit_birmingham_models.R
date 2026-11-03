# Fits the three-level trial models on the Birmingham LSOA data (lme4): null and full models for y_change and y_change_asinh,
# a constituency random intercept, and a strictly nested alternative. Run from datasets/ : Rscript ../tools/fit_birmingham_models.R birmingham_lsoa_2011_2021
suppressMessages({library(sf); library(lme4)})
args <- commandArgs(trailingOnly = TRUE)       # the dataset to fit, e.g. birmingham_lsoa_2011_2021
ds <- if (length(args)) args[1] else "birmingham_lsoa_2011_2021"
d <- st_drop_geometry(st_read(paste0(ds, ".gpkg"), quiet = TRUE))
y0 <- if (grepl("2001_2011", ds)) "2001" else "2011"; imd <- if (y0 == "2001") "imd_score_2010" else "imd_score_2019"
d$pct_ba_start <- d[[paste0("pct_ba_", y0)]]; d$msoa_div_start <- d[[paste0("msoa_div_", y0)]]; d$ward_social_start <- d[[paste0("ward_social_", y0)]]; d$imd_score <- d[[imd]]; d$pcon_labour <- d$pcon_labour_2019
sk <- function(x) mean((x - mean(x))^3) / sd(x)^3; ku <- function(x) mean((x - mean(x))^4) / sd(x)^4
vp <- function(m) { v <- as.data.frame(VarCorr(m)); setNames(round(v$vcov, 4), v$grp) }
icc <- function(m) { v <- as.data.frame(VarCorr(m)); round(setNames(v$vcov / sum(v$vcov), v$grp), 3) }
rep_fit <- function(lab, m) {
  cat("\n==", lab, "\n"); cat("singular:", isSingular(m), "  AIC:", round(AIC(m), 1), "\n")
  cat("variances:\n"); print(vp(m)); cat("share of variance:\n"); print(icc(m))
  r <- resid(m); cat("level-1 residuals: skew", round(sk(r), 2), " kurtosis", round(ku(r), 2), "\n")
  u <- ranef(m); for (g in names(u)) cat(g, "random effects (", nrow(u[[g]]), "): skew", round(sk(u[[g]][, 1]), 2), " kurtosis", round(ku(u[[g]][, 1]), 2), "\n")
}
cat("groups:", length(unique(d$msoa21cd)), "msoa,", length(unique(d$ward22cd)), "wards,", length(unique(d$pcon_cd)), "constituencies\n")
cat("MSOA x ward cells used:", nrow(unique(d[, c("msoa21cd", "ward22cd")])), " MSOAs spanning >1 ward:", sum(tapply(d$ward22cd, d$msoa21cd, function(x) length(unique(x))) > 1), "\n")
for (yv in c("y_change", "y_change_asinh")) {
  m0 <- lmer(as.formula(paste(yv, "~ 1 + (1 | msoa21cd) + (1 | ward22cd)")), d, REML = TRUE)
  rep_fit(paste("null,", yv, ", cross-classified msoa + ward"), m0)
}
f <- ". ~ pct_ba_start + imd_score + msoa_div_start + ward_social_start + pcon_labour + (1 | msoa21cd) + (1 | ward22cd)"
d[c("pct_ba_start","imd_score","msoa_div_start","ward_social_start","pcon_labour")] <- lapply(d[c("pct_ba_start","imd_score","msoa_div_start","ward_social_start","pcon_labour")], function(x) as.numeric(scale(x)))
for (yv in c("y_change", "y_change_asinh")) {
  m1 <- lmer(update(as.formula(paste(yv, "~ 1")), as.formula(f)), d, REML = TRUE)
  rep_fit(paste("full,", yv), m1); cat("fixed effects (standardised predictors):\n"); print(round(coef(summary(m1)), 3))
}
# add constituency as a further random intercept
m2 <- lmer(y_change_asinh ~ pct_ba_start + imd_score + msoa_div_start + ward_social_start + (1 | msoa21cd) + (1 | ward22cd) + (1 | pcon_cd), d, REML = TRUE)
rep_fit("asinh, plus constituency random intercept (10 groups), without the constituency predictor", m2)
# nested alternative: msoa within ward (msoa assigned to its main ward)
mm <- tapply(d$ward22cd, d$msoa21cd, function(x) names(sort(table(x), decreasing = TRUE))[1]); d$ward_main <- mm[d$msoa21cd]
m3 <- lmer(y_change_asinh ~ pct_ba_start + imd_score + (1 | ward_main/msoa21cd), d, REML = TRUE)
rep_fit("asinh, strictly nested msoa within main ward, X = pct_ba_start + imd_score", m3)
t0 <- system.time(for (i in 1:5) lmer(y_change_asinh ~ pct_ba_start + imd_score + (1 | msoa21cd) + (1 | ward22cd), d)); cat("\nmean fit time (s):", round(t0[["elapsed"]] / 5, 2), "\n")
