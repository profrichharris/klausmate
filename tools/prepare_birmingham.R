# Builds the Birmingham multilevel-modelling datasets in datasets/:
#   birmingham_lsoa.gpkg   639 LSOAs (the modelling rows), with the MSOA, ward and constituency variables copied down
#   birmingham_lsoa_table.csv   the same rows as a plain table (no geometry; join to the map on lsoa11cd)
#   birmingham_msoa.gpkg   132 MSOAs      birmingham_wards.gpkg   40 wards      birmingham_constituencies.gpkg   10 constituencies
#   birmingham_dictionary.csv   what every column means
#   Rscript tools/prepare_birmingham.R <source folder> <output folder>
#
# The source folder holds files fetched from these places (all Birmingham only, E08000025):
#   nomis2/   Nomis (https://www.nomisweb.co.uk/api/v01/dataset/<id>.data.csv, geography=E08000025TYPE<n>), Census
#             tables: NM_608_1 KS201EW ethnic group 2011 (rural_urban=0) at LSOA (TYPE298), MSOA (297) and ward (295),
#             NM_1606_1 KS006 ethnic group 2001 at 2001 output areas (310), NM_1626_1 KS018 tenure 2001 at 2001 OAs,
#             NM_619_1 KS402EW tenure 2011 at ward (295), and the 2011 usual residents of each 2011 OA (TYPE299, cell=0)
#   ons/      ONS Open Geography Portal lookups (ArcGIS feature services, LAD11CD = 'E08000025'): OA01_OA11_LAD11,
#             OA11_LSOA11_MSOA11_LAD11, OA11_WD11_LAD11 and WD17_PCON17_LAD17
#   imd/      English Indices of Deprivation 2019, File 7 (GOV.UK), kept for Birmingham: imd2019_birmingham.csv
#   bdy/      ONS boundaries as GeoJSON: lsoa11, msoa11 (BGC), ward11 (GCB) and pcon (Dec 2017 BGC)
#   elec/     birmingham_2010_results.csv, made from the Commons Library Election Results site (election pages 2647-2655, 3162)
args <- commandArgs(trailingOnly = TRUE)
src <- args[1]; out <- args[2]
library(sf)
rd  <- function(f) read.csv(file.path(src, f), stringsAsFactors = FALSE, check.names = FALSE)
mat <- function(f, geo = "GEOGRAPHY_CODE", cell) { d <- rd(f); xtabs(d$OBS_VALUE ~ d[[geo]] + d[[cell]]) }
Y_SCALE <- 0.1          # c in asinh(y / c), percentage points

# --- Lookups ------------------------------------------------------------------------------------------------------
oa    <- rd("ons/oa11_lsoa_msoa.csv")
oawd  <- rd("ons/oa11_wd11.csv")
wdpc  <- rd("ons/wd17_pcon17.csv")
lk    <- rd("ons/oa01_oa11.csv")
pop11 <- rd("nomis2/ks201_2011_oa_total.csv"); pop11 <- setNames(pop11$OBS_VALUE, pop11$GEOGRAPHY_CODE)
stopifnot(setequal(names(pop11), oa$OA11CD), setequal(oawd$WD11CD, wdpc$WD17CD))

# 2001 counts are held for 2001 output areas, which are split or merged in 2011 for a few. Each 2001 OA's count is
# shared among the 2011 OAs it became in proportion to their 2011 population (merged OAs simply add up).
lk$w   <- pop11[lk$OA11CD]
lk$wt  <- ifelse(ave(lk$w, lk$OA01CD, FUN = sum) > 0, lk$w / ave(lk$w, lk$OA01CD, FUN = sum), 1 / ave(lk$w, lk$OA01CD, FUN = length))
to_oa11 <- function(m) { stopifnot(all(lk$OA01CD %in% rownames(m)))
                         rowsum(m[lk$OA01CD, , drop = FALSE] * lk$wt, lk$OA11CD) }
up <- function(m11, key) rowsum(m11, key[rownames(m11)])      # sum 2011 OA rows to a larger area

# --- Ethnic group, in groups that exist in both censuses --------------------------------------------------------------
GROUPS <- c("white_british", "white_irish", "white_other", "mixed", "indian", "pakistani", "bangladeshi", "chinese",
            "other_asian", "black_african", "black_caribbean", "black_other", "other_ethnic")
e01 <- mat("nomis2/ks006_2001_oa.csv", cell = "C_ETHPUK11_CODE")
h01 <- cbind(total = e01[, "0"], white_british = e01[, "1"], white_irish = e01[, "2"], white_other = e01[, "3"],
             mixed = e01[, "4"] + e01[, "5"] + e01[, "6"] + e01[, "7"], indian = e01[, "8"], pakistani = e01[, "9"],
             bangladeshi = e01[, "10"], chinese = e01[, "15"], other_asian = e01[, "11"], black_african = e01[, "13"],
             black_caribbean = e01[, "12"], black_other = e01[, "14"], other_ethnic = e01[, "16"])
harm11 <- function(e) { k <- function(i) e[, sprintf("KS201EW%04d", i)]
  cbind(total = k(1), white_british = k(2), white_irish = k(3), white_other = k(4) + k(5),
        mixed = k(6) + k(7) + k(8) + k(9), indian = k(10), pakistani = k(11), bangladeshi = k(12), chinese = k(13),
        other_asian = k(14), black_african = k(15), black_caribbean = k(16), black_other = k(17),
        other_ethnic = k(18) + k(19)) }
eth01_oa <- to_oa11(h01)
stopifnot(abs(sum(eth01_oa[, "black_african"]) - sum(h01[, "black_african"])) < 1e-6)
eth11_lsoa <- harm11(mat("nomis2/ks201_2011_lsoa.csv", cell = "CELL_CODE"))
eth11_msoa <- harm11(mat("nomis2/ks201_2011_msoa.csv", cell = "CELL_CODE"))
eth11_ward <- harm11(mat("nomis2/ks201_2011_ward.csv", cell = "CELL_CODE"))
l_of <- setNames(oa$LSOA11CD, oa$OA11CD); m_of <- setNames(oa$MSOA11CD, oa$OA11CD)
w_of <- setNames(oawd$WD11CD, oawd$OA11CD)
eth01_lsoa <- up(eth01_oa, l_of); eth01_msoa <- up(eth01_oa, m_of)

# --- LSOA to MSOA, ward and constituency -------------------------------------------------------------------------
lsoa <- unique(oa[, c("LSOA11CD", "LSOA11NM", "MSOA11CD", "MSOA11NM")])
stopifnot(nrow(lsoa) == 639, !anyDuplicated(lsoa$LSOA11CD))
pick <- function(by, val) {      # the value that holds most of the residents of each LSOA
  d <- data.frame(l = l_of[names(pop11)], v = val[names(pop11)], p = pop11); a <- aggregate(p ~ l + v, d, sum)
  a <- a[order(a$l, -a$p), ]; a[!duplicated(a$l), ] }
wa <- pick(l_of, w_of); lsoa$WD11CD <- wa$v[match(lsoa$LSOA11CD, wa$l)]
nest <- tapply(w_of[names(pop11)], l_of[names(pop11)], function(x) length(unique(x)))
cat("LSOAs whose output areas fall in more than one ward:", sum(nest > 1), "of", length(nest), "\n")
lsoa$WD11NM <- oawd$WD11NM[match(lsoa$WD11CD, oawd$WD11CD)]
lsoa$PCON_CD <- wdpc$PCON17CD[match(lsoa$WD11CD, wdpc$WD17CD)]; lsoa$PCON_NM <- wdpc$PCON17NM[match(lsoa$WD11CD, wdpc$WD17CD)]
msoa_ward <- tapply(lsoa$WD11CD, lsoa$MSOA11CD, function(x) names(sort(table(x), decreasing = TRUE))[1])

# --- Outcome ------------------------------------------------------------------------------------------------------
N01 <- sum(h01[, "black_african"]); N11 <- sum(eth11_lsoa[, "black_african"])
outcome <- function(e01, e11, ids) {
  n01 <- e01[ids, "black_african"]; n11 <- e11[ids, "black_african"]
  t01 <- e01[ids, "total"]; t11 <- e11[ids, "total"]
  y <- 100 * (n11 / N11 - n01 / N01)
  data.frame(pop_2001 = round(t01, 2), pop_2011 = t11, ba_2001 = round(n01, 2), ba_2011 = n11,
             pct_ba_2001 = 100 * n01 / t01, pct_ba_2011 = 100 * n11 / t11,
             ba_share_2001 = 100 * n01 / N01, ba_share_2011 = 100 * n11 / N11,
             y_change = y, y_change_asinh = asinh(y / Y_SCALE), row.names = NULL)
}
simpson <- function(e) { p <- e[, GROUPS, drop = FALSE] / e[, "total"]; 1 - rowSums(p^2) }

# --- IMD 2019 (2011 LSOAs) -------------------------------------------------------------------------------------------
imd <- rd("imd/imd2019_birmingham.csv")
ii  <- match(lsoa$LSOA11CD, imd[["LSOA code (2011)"]]); stopifnot(!anyNA(ii))
lsoa$imd_score  <- imd[["Index of Multiple Deprivation (IMD) Score"]][ii]
lsoa$imd_rank   <- imd[["Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)"]][ii]
lsoa$imd_decile <- imd[["Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)"]][ii]
lsoa$pop_2015   <- imd[["Total population: mid 2015 (excluding prisoners)"]][ii]

# --- MSOA layer -----------------------------------------------------------------------------------------------------
msoa <- unique(lsoa[, c("MSOA11CD", "MSOA11NM")]); ids <- msoa$MSOA11CD
msoa <- cbind(msoa, WD11CD = unname(msoa_ward[ids]), n_lsoa = as.integer(table(lsoa$MSOA11CD)[ids]),
              outcome(eth01_msoa, eth11_msoa, ids))
msoa$imd_score <- tapply(lsoa$imd_score * lsoa$pop_2015, lsoa$MSOA11CD, sum)[ids] / tapply(lsoa$pop_2015, lsoa$MSOA11CD, sum)[ids]
msoa$msoa_div_2001 <- simpson(eth01_msoa)[ids]; msoa$msoa_div_2011 <- simpson(eth11_msoa)[ids]
msoa$msoa_div_chg  <- msoa$msoa_div_2011 - msoa$msoa_div_2001
stopifnot(abs(sum(msoa$y_change)) < 1e-9)

# --- Tenure (households), ward level --------------------------------------------------------------------------------
t01 <- mat("nomis2/ks018_2001_oa.csv", cell = "C_TENHUK11_CODE")
t01w <- up(to_oa11(cbind(total = t01[, "0"], social = t01[, "4"] + t01[, "5"], private = t01[, "6"])), w_of)
t11 <- mat("nomis2/ks402_2011_ward.csv", cell = "CELL_CODE"); k <- function(i) t11[, sprintf("KS402EW%04d", i)]
t11w <- cbind(total = k(1), social = k(5) + k(6), private = k(7))

# --- Ward layer ---------------------------------------------------------------------------------------------------
wards <- unique(wdpc[, c("WD17CD", "WD17NM", "PCON17CD", "PCON17NM")]); names(wards) <- c("WD11CD", "WD11NM", "PCON_CD", "PCON_NM")
wl <- lsoa$WD11CD; ids <- wards$WD11CD
ba <- function(e) cbind(total = e[, "total"], black_african = e[, "black_african"])
# LSOAs do not nest in wards (111 of 639 have output areas in more than one), so a ward's totals are built from its output
# areas (2011 from the Census ward table, 2001 from the allocated output areas), not by adding up LSOAs
e01_w <- up(eth01_oa, w_of)[ids, ]; e11_w <- eth11_ward[ids, ]
chk <- max(abs(rowsum(eth11_lsoa[lsoa$LSOA11CD, ], wl)[ids, "black_african"] - e11_w[, "black_african"]))
cat("Black African 2011 per ward: largest difference between adding up the LSOAs assigned to it and the Census ward table:", chk, "\n")
wards <- cbind(wards, n_lsoa = as.integer(table(wl)[ids]), outcome(e01_w, e11_w, ids))
wards$ward_social_2001  <- 100 * t01w[ids, "social"]  / t01w[ids, "total"]
wards$ward_social_2011  <- 100 * t11w[ids, "social"]  / t11w[ids, "total"]
wards$ward_social_chg   <- wards$ward_social_2011 - wards$ward_social_2001
wards$ward_private_2001 <- 100 * t01w[ids, "private"] / t01w[ids, "total"]
wards$ward_private_2011 <- 100 * t11w[ids, "private"] / t11w[ids, "total"]
wards$ward_private_chg  <- wards$ward_private_2011 - wards$ward_private_2001

# --- Constituency layer -----------------------------------------------------------------------------------------------
res <- rd("elec/birmingham_2010_results.csv")
pc  <- unique(wdpc[, c("PCON17CD", "PCON17NM")]); names(pc) <- c("PCON_CD", "PCON_NM"); ids <- pc$PCON_CD
w2p <- setNames(wards$PCON_CD, wards$WD11CD)      # constituencies are built from whole wards
e01_p <- rowsum(e01_w, w2p[rownames(e01_w)])[ids, ]; e11_p <- rowsum(e11_w, w2p[rownames(e11_w)])[ids, ]
pc <- cbind(pc, n_wards = as.integer(table(wards$PCON_CD)[ids]), outcome(e01_p, e11_p, ids))
ri <- match(pc$PCON_NM, res$name); stopifnot(!anyNA(ri))
pc$pcon_labour_2010  <- 100 * res$lab[ri] / res$valid_votes[ri]
pc$pcon_conservative_2010 <- 100 * res$con[ri] / res$valid_votes[ri]
pc$pcon_turnout_2010 <- res$turnout[ri]

# --- LSOA layer ------------------------------------------------------------------------------------------------------
ids <- lsoa$LSOA11CD
lsoa <- cbind(lsoa, outcome(eth01_lsoa, eth11_lsoa, ids))
mi <- match(lsoa$MSOA11CD, msoa$MSOA11CD); wi <- match(lsoa$WD11CD, wards$WD11CD); pi <- match(lsoa$PCON_CD, pc$PCON_CD)
for (v in c("msoa_div_2001", "msoa_div_2011", "msoa_div_chg")) lsoa[[v]] <- msoa[[v]][mi]
for (v in c("ward_social_2001", "ward_social_2011", "ward_social_chg", "ward_private_2001", "ward_private_2011", "ward_private_chg"))
  lsoa[[v]] <- wards[[v]][wi]
for (v in c("pcon_labour_2010", "pcon_conservative_2010", "pcon_turnout_2010")) lsoa[[v]] <- pc[[v]][pi]

# --- Boundaries ---------------------------------------------------------------------------------------------------
geo <- function(f, key, tab, tabkey) {
  g <- st_transform(st_read(file.path(src, f), quiet = TRUE), 27700)
  g <- g[match(tab[[tabkey]], g[[key]]), key]; stopifnot(!anyNA(st_dimension(g)))
  g <- st_set_geometry(tab, st_geometry(g)); st_geometry(g) <- "geometry"; g
}
low <- function(d) { names(d) <- tolower(names(d)); d }
sets <- list(
  birmingham_lsoa           = geo("bdy/lsoa11.geojson", "LSOA11CD", low(lsoa), "lsoa11cd"),
  birmingham_msoa           = geo("bdy/msoa11.geojson", "MSOA11CD", low(msoa), "msoa11cd"),
  birmingham_wards          = geo("bdy/ward11.geojson", "wd11cd",   low(wards), "wd11cd"),
  birmingham_constituencies = geo("bdy/pcon.geojson",   "PCON17CD", low(pc), "pcon_cd"))
for (nm in names(sets)) {
  g <- sets[[nm]]; names(g)[names(g) == "wd11cd"] <- "ward11cd"; names(g)[names(g) == "wd11nm"] <- "ward11nm"
  f <- file.path(out, paste0(nm, ".gpkg")); if (file.exists(f)) file.remove(f)
  st_write(g, f, layer = nm, quiet = TRUE, layer_options = "GEOMETRY_NAME=geometry")
  cat(nm, nrow(g), "rows,", ncol(g) - 1, "columns,", file.size(f) %/% 1024, "KB\n")
  if (nm == "birmingham_lsoa") {      # the same rows without geometry, to model without sf; join to the map on lsoa11cd
    write.csv(st_drop_geometry(g), file.path(out, "birmingham_lsoa_table.csv"), row.names = FALSE)
    cat("birmingham_lsoa_table", nrow(g), "rows\n")
  }
}
saveRDS(sets, file.path(src, "work_sets.rds"))

# --- Data dictionary ---------------------------------------------------------------------------------------------------
D <- c(
  lsoa11cd = "2011 Lower layer Super Output Area (LSOA) code (ONS)", lsoa11nm = "2011 LSOA name",
  msoa11cd = "2011 Middle layer Super Output Area (MSOA) code: every LSOA sits wholly inside one MSOA",
  msoa11nm = "2011 MSOA name",
  ward11cd = "2011 ward code. For LSOAs (and MSOAs) this is the ward that holds most of the area's residents: LSOAs and MSOAs do not nest exactly in wards (111 of the 639 LSOAs have output areas in more than one ward), so wards and MSOAs are partly crossed",
  ward11nm = "2011 ward name", pcon_cd = "Westminster parliamentary constituency code (2010-2024 boundaries); constituencies are built from whole wards",
  pcon_nm = "Constituency name", n_lsoa = "Number of LSOAs assigned to the area", n_wards = "Number of wards in the constituency",
  imd_score = "English Indices of Deprivation 2019 IMD score: higher means more deprived. For MSOAs, the mean of the LSOA scores weighted by their mid-2015 population",
  imd_rank = "IMD 2019 rank among the 32,844 LSOAs in England (1 = most deprived)",
  imd_decile = "IMD 2019 decile among LSOAs in England (1 = most deprived 10%)",
  pop_2015 = "Mid-2015 population estimate used by the IMD (excluding prisoners)",
  pop_2001 = "Usual residents, 2001 Census. Counts were published for 2001 output areas; where an output area was split or merged in 2011 its count is shared among the 2011 output areas in proportion to their 2011 population, so a few values are not whole numbers",
  pop_2011 = "Usual residents, 2011 Census",
  ba_2001 = "Black African residents, 2001 Census ('Black or Black British: Black African'); see pop_2001 for how counts reach 2011 areas",
  ba_2011 = "Black African residents, 2011 Census ('Black/African/Caribbean/Black British: African')",
  pct_ba_2001 = "Black African residents as a percentage of the area's residents, 2001 (the X variable)",
  pct_ba_2011 = "Black African residents as a percentage of the area's residents, 2011",
  ba_share_2001 = "n/N x 100 for 2001: the area's percentage of all of Birmingham's Black African residents (N = 6,191)",
  ba_share_2011 = "n/N x 100 for 2011: the area's percentage of all of Birmingham's Black African residents (N = 29,991)",
  y_change = "The outcome (Y): ba_share_2011 minus ba_share_2001, in percentage points of Birmingham's Black African population. Positive = the area gained share of the group, whatever happened to the group's total; it adds up to exactly 0 across the areas of a layer",
  y_change_asinh = "Second version of Y: asinh(y_change / 0.1). Zero still means no change and the sign is kept; values near zero are almost unchanged and large values are pulled in (skewness 0.96 and kurtosis 23 for y_change among LSOAs become about 0.0 and 3.3). The 0.1 is fixed for LSOA-sized values, so it compresses the larger MSOA, ward and constituency values more",
  msoa_div_2001 = "Ethnic diversity of the MSOA in 2001: Simpson's index, 1 minus the sum of squared shares of 13 ethnic groups that exist in both censuses (0 = one group, higher = more diverse; the maximum possible is 0.92)",
  msoa_div_2011 = "Ethnic diversity of the MSOA in 2011 (same groups and index)", msoa_div_chg = "msoa_div_2011 minus msoa_div_2001",
  ward_social_2001 = "Percentage of households in the ward renting from a council or housing association, 2001 (2001 output areas shared out to 2011 as for pop_2001, then added up by ward)",
  ward_social_2011 = "Percentage of households renting from a council or other social landlord, 2011", ward_social_chg = "ward_social_2011 minus ward_social_2001",
  ward_private_2001 = "Percentage of households renting from a private landlord or letting agency, 2001 (the 2001 'other' category is not counted)",
  ward_private_2011 = "Percentage of households renting from a private landlord or letting agency, 2011", ward_private_chg = "ward_private_2011 minus ward_private_2001",
  pcon_labour_2010 = "Labour's percentage of the valid votes in the constituency, general election 6 May 2010",
  pcon_conservative_2010 = "Conservative percentage of the valid votes, 2010", pcon_turnout_2010 = "Turnout (percentage of the electorate), 2010")
dict <- do.call(rbind, lapply(c(names(sets), "birmingham_lsoa_table"), function(nm) {
  cols <- sub("^wd11", "ward11", setdiff(names(sets[[sub("_table$", "", nm)]]), "geometry"))
  data.frame(dataset = nm, column = cols, description = unname(D[cols]), stringsAsFactors = FALSE)
}))
stopifnot(!anyNA(dict$description))
write.csv(dict, file.path(out, "birmingham_dictionary.csv"), row.names = FALSE, na = "")
cat("birmingham_dictionary:", nrow(dict), "rows\n")
