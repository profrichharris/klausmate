# Builds the Birmingham multilevel-modelling dataset in datasets/:
#   birmingham_lsoa.gpkg   659 LSOAs (2021) as an sf object: the modelling rows, with the MSOA, ward and constituency
#                          variables copied down. Numbers are rounded to 3 decimal places and counts to whole numbers.
#   Rscript tools/prepare_birmingham.R <source folder> <output folder>
#
# The outcome is the change between the 2011 and 2021 censuses (an earlier version used 2001 and 2011).
#
# The source folder holds files fetched from these places (all Birmingham only, E08000025):
#   nomis/    Nomis (https://www.nomisweb.co.uk/api/v01/dataset/<id>.data.csv, geography=E08000025TYPE<n>), Census tables:
#             NM_2041_1 TS021 ethnic group 2021 at OA (TYPE150), LSOA (151), MSOA (152) and 2022 ward (153);
#             NM_2072_1 TS054 tenure 2021 at 2022 ward (153);
#             NM_608_1 KS201EW ethnic group 2011 (rural_urban=0) and NM_619_1 KS402EW tenure 2011 at 2011 OA (299)
#   ons/      ONS Open Geography Portal lookups (ArcGIS feature services): OA11_OA21_LAD22_EW_LU_Exact_fit_V3,
#             OA21_LAD22_LSOA21_MSOA21_LEP22_EN_LU_V2, OA21_WD22_LTLA22_UTLA22_RGN22_CTRY22_EW_LU_v2 and OA21_PCON25_LAD21_EW_LU
#   imd/      English Indices of Deprivation 2025, File 7 (GOV.UK), kept for Birmingham: imd2025_birmingham.csv
#   bdy/      ONS Lower layer Super Output Areas (December 2021) BGC boundaries as GeoJSON: lsoa21.geojson
#   elec/     birmingham_2024_results.csv, made from candidacies.csv on the Commons Library Election Results site
#             (https://electionresults.parliament.uk/general-elections/6/candidacies.csv)
args <- commandArgs(trailingOnly = TRUE)
src <- args[1]; out <- args[2]
library(sf)
rd  <- function(f) read.csv(file.path(src, f), stringsAsFactors = FALSE, check.names = FALSE)
mat <- function(f, cell, geo = "GEOGRAPHY_CODE", keep = NULL) {
  d <- rd(f); if (!is.null(keep)) d <- d[d[[names(keep)]] == keep[[1]], ]
  unclass(xtabs(d$OBS_VALUE ~ d[[geo]] + d[[cell]]))
}
Y_SCALE <- 0.03         # c in asinh(y / c), percentage points (about the median size of a change)

# --- Lookups ------------------------------------------------------------------------------------------------------
oa    <- rd("ons/oa21_lsoa21_msoa21.csv")
oawd  <- rd("ons/oa21_wd22.csv")
oapc  <- rd("ons/oa21_pcon.csv")
lk    <- rd("ons/oa11_oa21.csv")
e21   <- mat("nomis/ts021_2021_oa.csv", "C2021_ETH_20_NAME")
pop21 <- e21[, "Total: All usual residents"]
stopifnot(setequal(rownames(e21), oa$OA21CD), setequal(oawd$OA21CD, oa$OA21CD), setequal(oapc$OA21CD, oa$OA21CD))

# 2011 counts are held for 2011 output areas, which are split or merged in 2021 for a few. Each 2011 OA's count is
# shared among the 2021 OAs it became in proportion to their 2021 population (merged OAs simply add up).
lk$w   <- pop21[lk$OA21CD]
lk$tot <- ave(lk$w, lk$OA11CD, FUN = sum)
lk$wt  <- ifelse(lk$tot > 0, lk$w / lk$tot, 1 / ave(lk$w, lk$OA11CD, FUN = length))
to_oa21 <- function(m) { stopifnot(all(lk$OA11CD %in% rownames(m)))
                         rowsum(m[lk$OA11CD, , drop = FALSE] * lk$wt, lk$OA21CD) }
up <- function(m21, key) rowsum(m21, key[rownames(m21)])      # sum 2021 OA rows to a larger area

# --- Ethnic group, in groups that exist in both censuses ----------------------------------------------------------------
GROUPS <- c("white_british", "white_irish", "white_other", "mixed", "indian", "pakistani", "bangladeshi", "chinese",
            "other_asian", "black_african", "black_caribbean", "black_other", "other_ethnic")
harm11 <- function(e) { k <- function(i) e[, sprintf("KS201EW%04d", i)]
  cbind(total = k(1), white_british = k(2), white_irish = k(3), white_other = k(4) + k(5),
        mixed = k(6) + k(7) + k(8) + k(9), indian = k(10), pakistani = k(11), bangladeshi = k(12), chinese = k(13),
        other_asian = k(14), black_african = k(15), black_caribbean = k(16), black_other = k(17),
        other_ethnic = k(18) + k(19)) }
harm21 <- function(e) { k <- function(s) e[, s]
  cbind(total = k("Total: All usual residents"),
        white_british = k("White: English, Welsh, Scottish, Northern Irish or British"), white_irish = k("White: Irish"),
        white_other = k("White: Gypsy or Irish Traveller") + k("White: Roma") + k("White: Other White"),
        mixed = k("Mixed or Multiple ethnic groups: White and Asian") + k("Mixed or Multiple ethnic groups: White and Black African") +
                k("Mixed or Multiple ethnic groups: White and Black Caribbean") + k("Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups"),
        indian = k("Asian, Asian British or Asian Welsh: Indian"), pakistani = k("Asian, Asian British or Asian Welsh: Pakistani"),
        bangladeshi = k("Asian, Asian British or Asian Welsh: Bangladeshi"), chinese = k("Asian, Asian British or Asian Welsh: Chinese"),
        other_asian = k("Asian, Asian British or Asian Welsh: Other Asian"),
        black_african = k("Black, Black British, Black Welsh, Caribbean or African: African"),
        black_caribbean = k("Black, Black British, Black Welsh, Caribbean or African: Caribbean"),
        black_other = k("Black, Black British, Black Welsh, Caribbean or African: Other Black"),
        other_ethnic = k("Other ethnic group: Arab") + k("Other ethnic group: Any other ethnic group")) }
eth11_oa11 <- harm11(mat("nomis/ks201_2011_oa.csv", "CELL_CODE"))
eth11_oa   <- to_oa21(eth11_oa11)
stopifnot(abs(sum(eth11_oa[, "black_african"]) - sum(eth11_oa11[, "black_african"])) < 1e-6)
eth21_oa   <- harm21(e21)
eth21_lsoa <- harm21(mat("nomis/ts021_2021_lsoa.csv", "C2021_ETH_20_NAME"))
eth21_msoa <- harm21(mat("nomis/ts021_2021_msoa.csv", "C2021_ETH_20_NAME"))
l_of <- setNames(oa$LSOA21CD, oa$OA21CD); m_of <- setNames(oa$MSOA21CD, oa$OA21CD)
w_of <- setNames(oawd$WD22CD, oawd$OA21CD); p_of <- setNames(oapc$PCON25CD, oapc$OA21CD)
eth11_lsoa <- up(eth11_oa, l_of); eth11_msoa <- up(eth11_oa, m_of)
# 2021 tables are published at every level but were altered independently to protect confidentiality, so adding up output
# areas need not give the published LSOA and MSOA figures exactly; the published figures are used at their own level
cat("Black African 2021, largest LSOA difference between adding up output areas and the LSOA table:",
    max(abs(up(eth21_oa, l_of)[rownames(eth21_lsoa), "black_african"] - eth21_lsoa[, "black_african"])), "\n")

# --- LSOA to MSOA, ward and constituency -------------------------------------------------------------------------
lsoa <- unique(oa[, c("LSOA21CD", "LSOA21NM", "MSOA21CD", "MSOA21NM")])
stopifnot(!anyDuplicated(lsoa$LSOA21CD), setequal(lsoa$LSOA21CD, rownames(eth21_lsoa)))
pick <- function(val) {      # the value that holds most of the residents of each LSOA
  d <- data.frame(l = l_of[names(pop21)], v = val[names(pop21)], p = pop21); a <- aggregate(p ~ l + v, d, sum)
  a <- a[order(a$l, -a$p), ]; a[!duplicated(a$l), ] }
wa <- pick(w_of); lsoa$WD22CD <- wa$v[match(lsoa$LSOA21CD, wa$l)]
pa <- pick(p_of); lsoa$PCON_CD <- pa$v[match(lsoa$LSOA21CD, pa$l)]
nest <- function(val) tapply(val[names(pop21)], l_of[names(pop21)], function(x) length(unique(x)))
cat("LSOAs whose output areas fall in more than one ward:", sum(nest(w_of) > 1), "of", nrow(lsoa),
    "; in more than one constituency:", sum(nest(p_of) > 1), "\n")
lsoa$WD22NM <- oawd$WD22NM[match(lsoa$WD22CD, oawd$WD22CD)]
lsoa$PCON_NM <- oapc$PCON25NM[match(lsoa$PCON_CD, oapc$PCON25CD)]

# --- Outcome ------------------------------------------------------------------------------------------------------
N11 <- sum(eth11_lsoa[, "black_african"]); N21 <- sum(eth21_lsoa[, "black_african"])
outcome <- function(e11, e21, ids) {
  n11 <- e11[ids, "black_african"]; n21 <- e21[ids, "black_african"]
  t11 <- e11[ids, "total"]; t21 <- e21[ids, "total"]
  y <- 100 * (n21 / N21 - n11 / N11)
  data.frame(pop_2011 = t11, pop_2021 = t21, ba_2011 = n11, ba_2021 = n21,
             pct_ba_2011 = 100 * n11 / t11, pct_ba_2021 = 100 * n21 / t21,
             ba_share_2011 = 100 * n11 / N11, ba_share_2021 = 100 * n21 / N21,
             y_change = y, y_change_asinh = asinh(y / Y_SCALE), row.names = NULL)
}
simpson <- function(e) { p <- e[, GROUPS, drop = FALSE] / e[, "total"]; 1 - rowSums(p^2) }

# --- IMD 2025 (2021 LSOAs) -------------------------------------------------------------------------------------------
imd <- rd("imd/imd2025_birmingham.csv")
ii  <- match(lsoa$LSOA21CD, imd[["LSOA code (2021)"]]); stopifnot(!anyNA(ii))
lsoa$imd_score  <- imd[["Index of Multiple Deprivation (IMD) Score"]][ii]
lsoa$imd_rank   <- imd[["Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)"]][ii]
lsoa$imd_decile <- imd[["Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)"]][ii]
lsoa$pop_2022   <- imd[["Total population: mid 2022"]][ii]

# --- MSOA diversity -------------------------------------------------------------------------------------------------
ids <- unique(lsoa$MSOA21CD)
msoa <- data.frame(MSOA21CD = ids, msoa_div_2011 = simpson(eth11_msoa)[ids], msoa_div_2021 = simpson(eth21_msoa)[ids])
msoa$msoa_div_chg <- msoa$msoa_div_2021 - msoa$msoa_div_2011

# --- Tenure (households), ward level --------------------------------------------------------------------------------
t11 <- mat("nomis/ks402_2011_oa.csv", "CELL_CODE", keep = c(RURAL_URBAN_CODE = 0)); k <- function(i) t11[, sprintf("KS402EW%04d", i)]
t11w <- up(to_oa21(cbind(total = k(1), social = k(5) + k(6), private = k(7) + k(8))), w_of)
t21 <- mat("nomis/ts054_2021_ward.csv", "C2021_TENURE_9_NAME")
t21w <- cbind(total = t21[, "Total: All households"], social = t21[, "Social rented"], private = t21[, "Private rented"])
ids <- unique(lsoa$WD22CD); stopifnot(all(ids %in% rownames(t21w)), all(ids %in% rownames(t11w)))
wards <- data.frame(WD22CD = ids,
  ward_social_2011  = 100 * t11w[ids, "social"]  / t11w[ids, "total"], ward_social_2021  = 100 * t21w[ids, "social"]  / t21w[ids, "total"],
  ward_private_2011 = 100 * t11w[ids, "private"] / t11w[ids, "total"], ward_private_2021 = 100 * t21w[ids, "private"] / t21w[ids, "total"])
wards$ward_social_chg  <- wards$ward_social_2021  - wards$ward_social_2011
wards$ward_private_chg <- wards$ward_private_2021 - wards$ward_private_2011

# --- Constituency vote (general election 4 July 2024) -----------------------------------------------------------------
res <- rd("elec/birmingham_2024_results.csv")
ri  <- match(lsoa$PCON_CD, res$code); stopifnot(!anyNA(ri))
pcv <- data.frame(pcon_labour_2024 = 100 * res$lab / res$valid_votes, pcon_conservative_2024 = 100 * res$con / res$valid_votes,
                  pcon_turnout_2024 = res$turnout)[ri, ]

# --- LSOA layer ------------------------------------------------------------------------------------------------------
lsoa <- cbind(lsoa, outcome(eth11_lsoa, eth21_lsoa, lsoa$LSOA21CD))
mi <- match(lsoa$MSOA21CD, msoa$MSOA21CD); wi <- match(lsoa$WD22CD, wards$WD22CD)
for (v in c("msoa_div_2011", "msoa_div_2021", "msoa_div_chg")) lsoa[[v]] <- msoa[[v]][mi]
for (v in c("ward_social_2011", "ward_social_2021", "ward_social_chg", "ward_private_2011", "ward_private_2021", "ward_private_chg"))
  lsoa[[v]] <- wards[[v]][wi]
lsoa <- cbind(lsoa, pcv)
stopifnot(abs(sum(lsoa$y_change)) < 1e-9, !anyNA(lsoa))

# --- Whole-number counts that keep their totals, then derived variables worked out from them ----------------------------
lr <- function(v) { f <- floor(v + 1e-9); n <- round(sum(v)) - sum(f)       # largest-remainder rounding
  if (n > 0) { i <- order(v - f, decreasing = TRUE)[seq_len(n)]; f[i] <- f[i] + 1 }; as.integer(f) }
lsoa$ba_2011 <- lr(lsoa$ba_2011); lsoa$pop_2011 <- lr(lsoa$pop_2011)
stopifnot(all(lsoa$ba_2011 <= lsoa$pop_2011), sum(lsoa$ba_2011) == round(N11))
lsoa$pct_ba_2011   <- 100 * lsoa$ba_2011 / lsoa$pop_2011
lsoa$ba_share_2011 <- 100 * lsoa$ba_2011 / sum(lsoa$ba_2011)
y <- lsoa$ba_share_2021 - lsoa$ba_share_2011
lsoa$y_change <- y; lsoa$y_change_asinh <- asinh(y / Y_SCALE)
lsoa$ba_2021 <- as.integer(lsoa$ba_2021); lsoa$pop_2021 <- as.integer(lsoa$pop_2021)
lsoa$imd_rank <- as.integer(lsoa$imd_rank); lsoa$imd_decile <- as.integer(lsoa$imd_decile); lsoa$pop_2022 <- as.integer(lsoa$pop_2022)
for (n in names(lsoa)) if (is.double(lsoa[[n]])) lsoa[[n]] <- round(lsoa[[n]], 3)
names(lsoa) <- tolower(names(lsoa))
names(lsoa)[names(lsoa) %in% c("wd22cd", "wd22nm")] <- c("ward22cd", "ward22nm")[match(names(lsoa)[names(lsoa) %in% c("wd22cd", "wd22nm")], c("wd22cd", "wd22nm"))]
first <- c("lsoa21cd", "lsoa21nm", "msoa21cd", "msoa21nm", "ward22cd", "ward22nm", "pcon_cd", "pcon_nm")
lsoa <- lsoa[, c(first, setdiff(names(lsoa), first))]

# --- Boundaries ---------------------------------------------------------------------------------------------------
g <- st_read(file.path(src, "bdy/lsoa21.geojson"), quiet = TRUE)
st_crs(g) <- NA_crs_; st_crs(g) <- 27700      # fetched with outSR=27700, so the coordinates are already British National Grid
g <- g[match(lsoa$lsoa21cd, g$LSOA21CD), ]; stopifnot(!anyNA(st_dimension(g)))
lsoa <- st_set_geometry(lsoa, st_geometry(g)); st_geometry(lsoa) <- "geometry"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
st_write(lsoa, file.path(out, "birmingham_lsoa.gpkg"), delete_dsn = TRUE, quiet = TRUE, layer = "birmingham_lsoa",
         layer_options = "GEOMETRY_NAME=geometry")
cat("Wrote", nrow(lsoa), "LSOAs,", ncol(lsoa), "columns to", file.path(out, "birmingham_lsoa.gpkg"), "\n")
cat("Black African 2011:", sum(lsoa$ba_2011), " 2021:", sum(lsoa$ba_2021), " population 2011:", sum(lsoa$pop_2011),
    " 2021:", sum(lsoa$pop_2021), "\n")
cat("y_change sums to", sum(lsoa$y_change), "; skew/kurtosis raw vs asinh:\n")
sk <- function(x) { z <- (x - mean(x)) / sd(x); c(skew = mean(z^3), kurt = mean(z^4)) }
print(rbind(raw = sk(lsoa$y_change), asinh = sk(lsoa$y_change_asinh)))
