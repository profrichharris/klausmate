# Klausmate online datasets

Data offered in Klausmate's **Load Package Data** window under *Klausmate (online datasets)*. The
files are kept here in their original formats and are **not** part of the R package (the folder is
excluded in `.Rbuildignore`). Each file carries its own licence: the MIT licence of the Klausmate
code does not apply to them.

`datasets.json` describes every dataset: title, description, file, format, size, SHA-256 checksum,
source, date retrieved, licence (and its link), the credit wording required, any notice that must
accompany it, and the date the licence was reviewed. Klausmate shows this beside each dataset and
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
| `birmingham_lsoa.gpkg` | ONS Census 2011 and 2021 (via Nomis), 2021 boundaries and lookups; MHCLG English Indices of Deprivation 2025; House of Commons Library 2024 election results. Built by `tools/prepare_birmingham.R` | OGL v3.0 (election results: Open Parliament Licence v3.0) |

### The Birmingham multilevel-modelling data

One sf layer of 659 **LSOAs** (2021 polygons) for fitting multilevel models (for example with lme4) and mapping the
results. The LSOAs sit in 132 **MSOAs**, 69 **wards** (2022) and 10 parliamentary **constituencies** (2024); the
higher-level variables are copied down onto each LSOA, with the MSOA, ward and constituency codes and names
(`msoa21cd`, `ward22cd`, `pcon_cd`) to use as grouping factors. LSOAs and MSOAs only roughly nest in wards (165 of the
659 LSOAs have output areas in more than one ward, 52 in more than one constituency), so each is given the ward and
constituency holding most of its residents, and MSOA and ward are partly crossed. Hodge Hill and Solihull North also
covers part of Solihull, outside Birmingham, so its 2024 vote includes voters outside the data. Counts are whole numbers
and the other numbers are rounded to three decimal places. Use `sf::st_drop_geometry()` to model without the map.

- Outcome `y_change`: the change from 2011 to 2021 in the percentage of Birmingham's Black African population who live
  in the LSOA (n/N x 100, where n is the LSOA's Black African residents and N the city's), in percentage points; adds up to 0.
  It is not the percentage of the LSOA's own residents (that is `pct_ba_*`), nor a percentage change in numbers.
  `y_change_asinh` is `asinh(y_change / 0.03)`: zero stays zero, the sign is kept and the heavy tails are pulled in (skew 2.0 to 0.1, kurtosis 20.6 to 2.9).
- Predictors: `pct_ba_2011` (percentage Black African, 2011) and `imd_score` (IMD 2025, at LSOA); `msoa_div_*` (ethnic
  diversity, MSOA); `ward_social_*` and `ward_private_*` (tenure, ward); `pcon_labour_2024`, `pcon_conservative_2024` and
  `pcon_turnout_2024`.
- 2011 counts exist for 2011 output areas; they are shared out to 2021 output areas in proportion to 2021 population
  where an area was split (merged areas add up). The few LSOAs left with fractional 2011 counts are rounded to whole
  numbers so that the totals are kept (29,991 Black African; 1,073,045 people), and the percentages, shares and
  `y_change` are worked out from the rounded counts. The 2021 counts are as published for each LSOA.

Not yet included: the Rogers Park residential building outlines (OpenStreetMap, ODbL): they need the
"(c) OpenStreetMap contributors" credit and share-alike notice written up first.
