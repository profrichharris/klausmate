# Builds the Birmingham multilevel-modelling datasets in datasets/:
#   birmingham_lsoa_2001_2011.gpkg   659 LSOAs (2021 geography) as an sf object: the change in the share of the city's
#   birmingham_lsoa_2011_2021.gpkg   Black African population between two censuses, with predictors. Both use the SAME
#                                    LSOAs, MSOAs, 2022 wards and 2024 constituencies, so that variance shares in a null
#                                    multilevel model can be compared, and the SAME transformation of the outcome.
#   Numbers are rounded to 3 decimal places and counts to whole numbers.
#   Rscript tools/prepare_birmingham.R <source folder> <output folder>
#
# 2001 and 2011 counts (held for 2001 / 2011 output areas) are shared out to 2021 output areas in proportion to
# population (2001 -> 2011 -> 2021), then summed to the 2021 LSOAs, MSOAs and 2022 wards.
#
# The source folder holds files fetched from these places (all Birmingham only, E08000025):
#   nomis/    Nomis (https://www.nomisweb.co.uk/api/v01/dataset/<id>.data.csv, geography=E08000025TYPE<n>), Census tables:
#             NM_2041_1 TS021 ethnic group 2021 at OA (TYPE150), LSOA (151), MSOA (152) and 2022 ward (153);
#             NM_2072_1 TS054 tenure 2021 at 2022 ward (153);
#             NM_608_1 KS201EW ethnic group 2011 (rural_urban=0) and NM_619_1 KS402EW tenure 2011 at 2011 OA (299);
#             NM_1606_1 KS006 ethnic group 2001 (ks006_2001_oa.csv) and NM_1626_1 KS018 tenure 2001 (ks018_2001_oa.csv) at 2001 OA (310)
#   ons/      ONS Open Geography Portal lookups (ArcGIS feature services): OA01_OA11_LAD11 (oa01_oa11.csv),
#             OA11_OA21_LAD22_EW_LU_Exact_fit_V3, OA11_LSOA_MSOA (oa11_lsoa_msoa.csv), OA01_LSOA01_MSOA01 (oa01_lsoa01.csv),
#             OA21_LAD22_LSOA21_MSOA21_LEP22_EN_LU_V2, OA21_WD22_LTLA22_UTLA22_RGN22_CTRY22_EW_LU_v2 and OA21_PCON25_LAD21_EW_LU
#   imd/      English Indices of Deprivation, kept for Birmingham: 2010 (File 1871702, 2001 LSOAs: imd2010_birmingham.csv),
#             2019 (File 7, 2011 LSOAs: imd2019_birmingham.csv) and 2025 (File 7, 2021 LSOAs: imd2025_birmingham.csv)
#   bdy/      ONS Lower layer Super Output Areas (December 2021) BGC boundaries as GeoJSON: lsoa21.geojson
#   elec/     birmingham_2024_results.csv and birmingham_2019n_results.csv (2019 notional result on the 2024 boundaries), made
#             from the candidacies.csv of general elections 6 and 5 on the Commons Library Election Results site
#             (https://electionresults.parliament.uk/general-elections/<n>/candidacies.csv)
args <- commandArgs(trailingOnly = TRUE)
src <- args[1]; out <- args[2]
library(sf)
rd  <- function(f) read.csv(file.path(src, f), stringsAsFactors = FALSE, check.names = FALSE)
mat <- function(f, cell, geo = "GEOGRAPHY_CODE", keep = NULL) {
  d <- rd(f); if (!is.null(keep)) d <- d[d[[names(keep)]] == keep[[1]], ]
  unclass(xtabs(d$OBS_VALUE ~ d[[geo]] + d[[cell]]))
}
IMD_GROUPS <- 20        # number of equal-sized groups of Birmingham LSOAs for the 2010 / 2019 / 2025 IMD scores
Y_SCALE <- as.numeric(Sys.getenv("Y_SCALE", "0.05"))   # c in asinh(y / c), percentage points; the same in both datasets

# --- Lookups ------------------------------------------------------------------------------------------------------
oa    <- rd("ons/oa21_lsoa21_msoa21.csv")
oawd  <- rd("ons/oa21_wd22.csv")
oapc  <- rd("ons/oa21_pcon.csv")
lk11  <- rd("ons/oa11_oa21.csv")                 # 2011 -> 2021 output areas
lk01  <- rd("ons/oa01_oa11.csv")                 # 2001 -> 2011 output areas
e21   <- mat("nomis/ts021_2021_oa.csv", "C2021_ETH_20_NAME")
pop21 <- e21[, "Total: All usual residents"]
stopifnot(setequal(rownames(e21), oa$OA21CD), setequal(oawd$OA21CD, oa$OA21CD), setequal(oapc$OA21CD, oa$OA21CD))
e11   <- mat("nomis/ks201_2011_oa.csv", "CELL_CODE")
pop11 <- e11[, "KS201EW0001"]

# Counts held for an older output area, which was split or merged in a later census, are shared among the later areas it
# became in proportion to their (later) population; merged areas simply add up.
weights <- function(lk, from, to, pop) {
  lk$w <- pop[lk[[to]]]; lk$tot <- ave(lk$w, lk[[from]], FUN = sum)
  lk$wt <- ifelse(lk$tot > 0, lk$w / lk$tot, 1 / ave(lk$w, lk[[from]], FUN = length)); lk }
lk11 <- weights(lk11, "OA11CD", "OA21CD", pop21)
lk01 <- weights(lk01, "OA01CD", "OA11CD", pop11)
move <- function(m, lk, from, to) { stopifnot(all(lk[[from]] %in% rownames(m)))
                                    rowsum(m[lk[[from]], , drop = FALSE] * lk$wt, lk[[to]]) }
to_oa21_from11 <- function(m) move(m, lk11, "OA11CD", "OA21CD")
to_oa21_from01 <- function(m) to_oa21_from11(move(m, lk01, "OA01CD", "OA11CD"))
# an intensive measure (a score) is carried across as a population-weighted mean of the areas that contribute to each new area
carry <- function(score, pop, lk, from, to) {
  num <- rowsum(score[lk[[from]]] * pop[lk[[from]]] * lk$wt, lk[[to]]); den <- rowsum(pop[lk[[from]]] * lk$wt, lk[[to]])
  setNames(num[, 1] / den[, 1], rownames(num)) }
up <- function(m21, key) rowsum(m21, key[rownames(m21)])      # sum 2021 OA rows to a larger area
l_of <- setNames(oa$LSOA21CD, oa$OA21CD); m_of <- setNames(oa$MSOA21CD, oa$OA21CD)
w_of <- setNames(oawd$WD22CD, oawd$OA21CD); p_of <- setNames(oapc$PCON25CD, oapc$OA21CD)

# --- Ethnic group, in groups that exist in all three censuses -----------------------------------------------------------
GROUPS <- c("white_british", "white_irish", "white_other", "mixed", "indian", "pakistani", "bangladeshi", "chinese",
            "other_asian", "black_african", "black_caribbean", "black_other", "other_ethnic")
e01 <- mat("nomis/ks006_2001_oa.csv", "C_ETHPUK11_CODE")
h01 <- cbind(total = e01[, "0"], white_british = e01[, "1"], white_irish = e01[, "2"], white_other = e01[, "3"],
             mixed = e01[, "4"] + e01[, "5"] + e01[, "6"] + e01[, "7"], indian = e01[, "8"], pakistani = e01[, "9"],
             bangladeshi = e01[, "10"], chinese = e01[, "15"], other_asian = e01[, "11"], black_african = e01[, "13"],
             black_caribbean = e01[, "12"], black_other = e01[, "14"], other_ethnic = e01[, "16"])
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
eth <- list()
eth[["2001"]] <- to_oa21_from01(h01)
eth[["2011"]] <- to_oa21_from11(harm11(e11))
stopifnot(abs(sum(eth[["2001"]][, "black_african"]) - sum(h01[, "black_african"])) < 1e-6,
          abs(sum(eth[["2011"]][, "black_african"]) - sum(harm11(e11)[, "black_african"])) < 1e-6)
eth21_lsoa <- harm21(mat("nomis/ts021_2021_lsoa.csv", "C2021_ETH_20_NAME"))
eth21_msoa <- harm21(mat("nomis/ts021_2021_msoa.csv", "C2021_ETH_20_NAME"))
eth_lsoa <- list("2001" = up(eth[["2001"]], l_of), "2011" = up(eth[["2011"]], l_of), "2021" = eth21_lsoa)
eth_msoa <- list("2001" = up(eth[["2001"]], m_of), "2011" = up(eth[["2011"]], m_of), "2021" = eth21_msoa)
# 2021 tables are published at every level but were altered independently to protect confidentiality, so adding up output
# areas need not give the published LSOA and MSOA figures exactly; the published figures are used at their own level
cat("Black African 2021, largest LSOA difference between adding up output areas and the LSOA table:",
    max(abs(up(harm21(e21), l_of)[rownames(eth21_lsoa), "black_african"] - eth21_lsoa[, "black_african"])), "\n")

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

# --- Counts, as whole numbers that keep their totals --------------------------------------------------------------------
lr <- function(v) { f <- floor(v + 1e-9); n <- round(sum(v)) - sum(f)       # largest-remainder rounding
  if (n > 0) { i <- order(v - f, decreasing = TRUE)[seq_len(n)]; f[i] <- f[i] + 1 }; as.integer(f) }
ids <- lsoa$LSOA21CD
cnt <- list()
for (yr in c("2001", "2011", "2021")) {
  b <- eth_lsoa[[yr]][ids, "black_african"]; t <- eth_lsoa[[yr]][ids, "total"]
  cnt[[yr]] <- list(ba = if (yr == "2021") as.integer(b) else lr(b), pop = if (yr == "2021") as.integer(t) else lr(t))
  stopifnot(all(cnt[[yr]]$ba <= cnt[[yr]]$pop))
}
simpson <- function(e) { p <- e[, GROUPS, drop = FALSE] / e[, "total"]; 1 - rowSums(p^2) }

# --- IMD: 2010 (2001 LSOAs), 2019 (2011 LSOAs), 2025 (2021 LSOAs) -----------------------------------------------------
# 2010 and 2019 scores are carried to the 2021 LSOAs through output areas (a population-weighted mean where LSOAs changed)
pop01 <- h01[, "total"]
imd10 <- rd("imd/imd2010_birmingham.csv"); s10 <- setNames(as.numeric(imd10[["IMD SCORE"]]), imd10[["LSOA CODE"]])
o2l01 <- rd("ons/oa01_lsoa01.csv")      # this lookup uses the original 2001 output area codes (OA01CDO)
o2l01$OA01CD <- lk01$OA01CD[match(o2l01$OA01CD, lk01$OA01CDO)]; stopifnot(!anyNA(o2l01$OA01CD))
sc01 <- s10[o2l01$LSOA01CD[match(names(pop01), o2l01$OA01CD)]]; names(sc01) <- names(pop01)
sc11a <- carry(sc01, pop01, lk01, "OA01CD", "OA11CD")
imd19 <- rd("imd/imd2019_birmingham.csv")
s19 <- setNames(imd19[["Index of Multiple Deprivation (IMD) Score"]], imd19[["LSOA code (2011)"]])
o2l11 <- rd("ons/oa11_lsoa_msoa.csv"); sc11b <- s19[o2l11$LSOA11CD[match(names(pop11), o2l11$OA11CD)]]; names(sc11b) <- names(pop11)
to_lsoa21 <- function(sc_oa21) { sc_oa21 <- sc_oa21[names(pop21)]
  (rowsum(sc_oa21 * pop21, l_of[names(pop21)]) / rowsum(pop21, l_of[names(pop21)]))[ids, 1] }
lsoa$imd_score_2010 <- unname(to_lsoa21(carry(sc11a, pop11, lk11, "OA11CD", "OA21CD")))
lsoa$imd_score_2019 <- unname(to_lsoa21(carry(sc11b, pop11, lk11, "OA11CD", "OA21CD")))
cat("IMD score 2010 / 2019 / 2025 correlations (LSOAs):\n")
imd25 <- rd("imd/imd2025_birmingham.csv")
ii  <- match(ids, imd25[["LSOA code (2021)"]]); stopifnot(!anyNA(ii))
lsoa$imd_score_2025  <- imd25[["Index of Multiple Deprivation (IMD) Score"]][ii]
lsoa$imd_rank_2025   <- as.integer(imd25[["Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)"]][ii])
lsoa$imd_decile_2025 <- as.integer(imd25[["Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)"]][ii])
lsoa$pop_2022        <- as.integer(imd25[["Total population: mid 2022"]][ii])
print(round(cor(lsoa[, c("imd_score_2010", "imd_score_2019", "imd_score_2025")]), 3))

# --- MSOA diversity -------------------------------------------------------------------------------------------------
mids <- unique(lsoa$MSOA21CD)
div <- sapply(c("2001", "2011", "2021"), function(y) simpson(eth_msoa[[y]])[mids])
mi <- match(lsoa$MSOA21CD, mids)

# --- Tenure (households), ward level: private rented means a private landlord or letting agency in every census ---------
t01 <- mat("nomis/ks018_2001_oa.csv", "C_TENHUK11_CODE")
t01w <- up(to_oa21_from01(cbind(total = t01[, "0"], social = t01[, "4"] + t01[, "5"], private = t01[, "6"])), w_of)
t11 <- mat("nomis/ks402_2011_oa.csv", "CELL_CODE", keep = c(RURAL_URBAN_CODE = 0)); k <- function(i) t11[, sprintf("KS402EW%04d", i)]
t11w <- up(to_oa21_from11(cbind(total = k(1), social = k(5) + k(6), private = k(7))), w_of)
t21 <- mat("nomis/ts054_2021_ward.csv", "C2021_TENURE_9_NAME")
t21w <- cbind(total = t21[, "Total: All households"], social = t21[, "Social rented"],
              private = t21[, "Private rented: Private landlord or letting agency"])
wids <- unique(lsoa$WD22CD); stopifnot(all(wids %in% rownames(t21w)), all(wids %in% rownames(t11w)), all(wids %in% rownames(t01w)))
tw <- function(m, col) 100 * m[wids, col] / m[wids, "total"]
ten <- list(social = sapply(list("2001" = t01w, "2011" = t11w, "2021" = t21w), tw, "social"),
            private = sapply(list("2001" = t01w, "2011" = t11w, "2021" = t21w), tw, "private"))
wi <- match(lsoa$WD22CD, wids)

# --- Constituency vote: the 2019 result (notional, on the 2024 boundaries) and the 2024 result --------------------------
vote <- function(f, yr) { res <- rd(f); ri <- match(lsoa$PCON_CD, res$code); stopifnot(!anyNA(ri))
  setNames(data.frame(100 * res$lab / res$valid_votes, 100 * res$con / res$valid_votes, res$turnout)[ri, ],
           paste0(c("pcon_labour_", "pcon_conservative_", "pcon_turnout_"), yr)) }
votes <- cbind(vote("elec/birmingham_2019n_results.csv", 2019), vote("elec/birmingham_2024_results.csv", 2024))

# --- Boundaries --------------------------------------------------------------------------------------------------------
g <- st_read(file.path(src, "bdy/lsoa21.geojson"), quiet = TRUE)
st_crs(g) <- NA_crs_; st_crs(g) <- 27700      # fetched with outSR=27700, so the coordinates are already British National Grid
g <- g[match(ids, g$LSOA21CD), ]; stopifnot(!anyNA(st_dimension(g)))

# --- One dataset per period ------------------------------------------------------------------------------------------
build <- function(y0, y1, imd_year) {
  b0 <- cnt[[y0]]$ba; b1 <- cnt[[y1]]$ba; p0 <- cnt[[y0]]$pop; p1 <- cnt[[y1]]$pop
  s0 <- 100 * b0 / sum(b0); s1 <- 100 * b1 / sum(b1); y <- s1 - s0
  d <- lsoa[, c("LSOA21CD", "LSOA21NM", "MSOA21CD", "MSOA21NM", "WD22CD", "WD22NM", "PCON_CD", "PCON_NM")]
  d[[paste0("imd_score_", imd_year)]] <- lsoa[[paste0("imd_score_", imd_year)]]
  # the same score as twentieths (20 equal-sized groups) of Birmingham's own LSOAs, 1 = the most deprived twentieth (as with IMD deciles)
  sc <- lsoa[[paste0("imd_score_", imd_year)]]
  d[[paste0("imd_q20_", imd_year)]] <- as.integer(floor(IMD_GROUPS * (rank(-sc, ties.method = "min") - 1) / length(sc)) + 1)
  d[c("imd_score_2025", "imd_rank_2025", "imd_decile_2025")] <- lsoa[c("imd_score_2025", "imd_rank_2025", "imd_decile_2025")]
  d$imd_q20_2025 <- as.integer(floor(IMD_GROUPS * (rank(-lsoa$imd_score_2025, ties.method = "min") - 1) / nrow(lsoa)) + 1)
  d$pop_2022 <- lsoa$pop_2022
  d[[paste0("pop_", y0)]] <- p0; d[[paste0("pop_", y1)]] <- p1; d[[paste0("ba_", y0)]] <- b0; d[[paste0("ba_", y1)]] <- b1
  d[[paste0("pct_ba_", y0)]] <- 100 * b0 / p0; d[[paste0("pct_ba_", y1)]] <- 100 * b1 / p1
  d[[paste0("ba_share_", y0)]] <- s0; d[[paste0("ba_share_", y1)]] <- s1
  d$y_change <- y; d$y_change_asinh <- asinh(y / Y_SCALE)
  d[[paste0("msoa_div_", y0)]] <- div[mi, y0]; d[[paste0("msoa_div_", y1)]] <- div[mi, y1]; d$msoa_div_chg <- div[mi, y1] - div[mi, y0]
  for (v in c("social", "private")) {
    d[[paste0("ward_", v, "_", y0)]] <- ten[[v]][wi, y0]; d[[paste0("ward_", v, "_", y1)]] <- ten[[v]][wi, y1]
    d[[paste0("ward_", v, "_chg")]] <- ten[[v]][wi, y1] - ten[[v]][wi, y0] }
  d <- cbind(d, votes)
  stopifnot(abs(sum(y)) < 1e-9, !anyNA(d))
  for (n in names(d)) if (is.double(d[[n]])) d[[n]] <- round(d[[n]], 3)
  names(d) <- tolower(names(d))
  names(d)[names(d) %in% c("wd22cd", "wd22nm")] <- c("ward22cd", "ward22nm")[match(names(d)[names(d) %in% c("wd22cd", "wd22nm")], c("wd22cd", "wd22nm"))]
  d <- st_set_geometry(d, st_geometry(g)); st_geometry(d) <- "geometry"; d
}
sk <- function(x) { z <- (x - mean(x)) / sd(x); c(skew = mean(z^3), kurt = mean(z^4)) }
dir.create(out, showWarnings = FALSE, recursive = TRUE)
for (p in list(list("2001", "2011", "2010"), list("2011", "2021", "2019"))) {
  d <- build(p[[1]], p[[2]], p[[3]]); nm <- sprintf("birmingham_lsoa_%s_%s", p[[1]], p[[2]])
  st_write(d, file.path(out, paste0(nm, ".gpkg")), delete_dsn = TRUE, quiet = TRUE, layer = nm, layer_options = "GEOMETRY_NAME=geometry")
  cat("\n", nm, ":", nrow(d), "LSOAs,", ncol(d), "columns\n")
  cat("Black African", p[[1]], ":", sum(d[[paste0("ba_", p[[1]])]]), " ", p[[2]], ":", sum(d[[paste0("ba_", p[[2]])]]), "\n")
  cat("y_change sums to", sum(d$y_change), "; skew/kurtosis raw vs asinh (c =", Y_SCALE, "):\n")
  print(rbind(raw = sk(d$y_change), asinh = sk(d$y_change_asinh)))
  cat("kurtosis of asinh(y/c) for other c:", paste(sprintf("%g: %.2f", c(0.02, 0.03, 0.05, 0.075, 0.1, 0.15),
      sapply(c(0.02, 0.03, 0.05, 0.075, 0.1, 0.15), function(cc) sk(asinh(d$y_change / cc))[2])), collapse = "  "), "\n")
}
