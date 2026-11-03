# Klausmate online datasets

Data offered in Klausmate's **Load Package Data** window under *Klausmate (online datasets)*. The
files are kept here in their original formats and are **not** part of the R package (the folder is
excluded in `.Rbuildignore`). Each file carries its own licence: the MIT licence of the Klausmate
code does not apply to them.

`datasets.json` describes every dataset: title, description, file, format, size, SHA-256 checksum,
source, date retrieved, licence (and its link), the credit wording required, any notice that must
accompany it, and the date the licence was reviewed. An entry may also have a `variables` list
(`name`, `group`, `description` for each column), shown in the Look-at window's Variable description tab; the check
below confirms that every column is described and nothing else. Klausmate shows this beside each dataset and
adds the credit and licence as comments to the code log when the data are downloaded.

## Adding a dataset

1. Check the licence yourself: the data must be free to redistribute (public domain, CC0, CC-BY, or
   ODbL with its share-alike wording in `notice`, OGL, or published free to download with no formal licence, `free-with-credit`, which needs a `notice` saying so). `tools/prepare_childhood_lottery.R` shows how the Childhood Lottery files were built. Prefer plain formats (`.gpkg`, `.geojson`,
   `.csv`); avoid `.rds` / `.RData`. Name the geometry column `geometry` (write GeoPackages with
   `layer_options = "GEOMETRY_NAME=geometry"`): sf's default `geom` makes `st_area(geometry)` find R's
   `geometry()` function instead of the column.
2. Put the file in this folder and add an entry to `datasets.json` (copy an existing one). Fill in
   every field; set `reviewed` to the date you checked the licence.
3. Run `Rscript tools/check_datasets.R --update` from the package root to fill in the size and
   checksum, then `Rscript tools/check_datasets.R` to confirm everything passes. The check covers
   the paperwork (fields present, licence on the approved list, files and checksums current); it
   cannot decide whether a licence really allows redistribution.
4. Commit and push. The files are fetched from
   `https://raw.githubusercontent.com/profrichharris/klausmate/main/datasets/`.

## Current datasets

| File | Source | Licence |
|------|--------|---------|
| `chicago_tracts_income.gpkg` | US Census Bureau, ACS 2018-2022 5-year estimates (B19013) with 2022 TIGER/Line tracts, Chicago | Public domain (US federal government work) |
| `chicago_boundary.gpkg` | US Census Bureau, 2022 TIGER/Line cartographic boundary file (places), Chicago | Public domain (US federal government work) |
| `childhood_lottery.gpkg`, `childhood_lottery_metrics.csv` | Children's Commissioner for England, The Childhood Lottery (2026); ONS Dec 2025 UTLA boundaries | Free to download, credit required (no formal licence stated); boundaries OGL v3.0 |
| `birmingham_lsoa_2001_2011.gpkg`, `birmingham_lsoa_2011_2021.gpkg` | ONS Census 2001, 2011 and 2021 (via Nomis), 2021 boundaries and lookups; English Indices of Deprivation 2010, 2019 and 2025; House of Commons Library 2019 (notional) and 2024 election results. Built by `tools/prepare_birmingham.R` | OGL v3.0 (election results: Open Parliament Licence v3.0) |

### The Birmingham multilevel-modelling data

Two sf layers of the same 659 **LSOAs** (2021 polygons), one for the change from 2001 to 2011 (`birmingham_lsoa_2001_2011`)
and one for 2011 to 2021 (`birmingham_lsoa_2011_2021`), for fitting multilevel models (for example with lme4) and mapping the
results. Both use the **same geography**, so the variance at each level can be compared between the two periods: the LSOAs
sit in 132 **MSOAs**, 69 **wards** (2022) and 10 parliamentary **constituencies** (2024), given as `msoa21cd`, `ward22cd`
and `pcon_cd` to use as grouping factors; the higher-level variables are copied down onto each LSOA. LSOAs and MSOAs only
roughly nest in wards (165 of the 659 LSOAs have output areas in more than one ward, 52 in more than one constituency), so
each is given the ward and constituency holding most of its residents, and MSOA and ward are partly crossed. Hodge Hill and
Solihull North also covers part of Solihull, outside Birmingham, so its votes include voters outside the data. Counts are
whole numbers and the other numbers are rounded to three decimal places. Use `sf::st_drop_geometry()` to model without the map.

- Outcome `y_change`: the change in the percentage of Birmingham's Black African population who live in the LSOA
  (n/N x 100, where n is the LSOA's Black African residents and N the city's), in percentage points; adds up to 0. It is not
  the percentage of the LSOA's own residents (that is `pct_ba_*`), nor a percentage change in numbers. `y_change_asinh` is
  `asinh(y_change / 0.05)`, the same transformation in both datasets: zero stays zero, the sign is kept and the heavy tails
  are pulled in (kurtosis 11.0 to 2.3 for 2001-2011 and 20.6 to 3.6 for 2011-2021).
- Predictors (years follow the period): `pct_ba_*` (percentage Black African, start and end census) and the IMD score for a
  year near the later census, `imd_score_2010` (2001-2011 file) or `imd_score_2019` (2011-2021 file), and `imd_score_2025`
  with its rank and decile in both; `msoa_div_*` (ethnic diversity, MSOA); `ward_social_*` and `ward_private_*` (tenure, ward;
  private rented means a private landlord or letting agency in every census); and `pcon_labour_*`, `pcon_conservative_*` and
  `pcon_turnout_*` for the 2019 (notional, on the 2024 boundaries) and 2024 general elections, the same in both files.
- Counts for 2001 and 2011 exist for older output areas; they are shared out to 2021 output areas in proportion to
  population (2001 to 2011 to 2021) where an area was split (merged areas add up). The few LSOAs left with fractional counts
  are rounded to whole numbers so that the totals are kept (Black African 6,191 in 2001, 29,991 in 2011, 66,851 in 2021), and
  the percentages, shares and `y_change` are worked out from the rounded counts. The 2021 counts are as published for each LSOA.
  The 2010 and 2019 IMD scores are carried to the 2021 LSOAs through output areas (a population-weighted mean where LSOAs changed),
  so there is no national rank or decile for them. Instead, `imd_q20_2010` or `imd_q20_2019` (one per file) and `imd_q20_2025`
  (both files) put the score into twenty equal-sized groups of Birmingham's own LSOAs (33 or 32 each), 1 being the most
  deprived twentieth, to use as a grouping factor in a multilevel model.
- Every column is described in plain English in the `variables` list of its entry in `datasets.json` (name, theme, description).
  Klausmate shows these in a Variable description tab when a student looks at the dataset with the binoculars button.

Not yet included: the Rogers Park residential building outlines (OpenStreetMap, ODbL): they need the
"(c) OpenStreetMap contributors" credit and share-alike notice written up first.
