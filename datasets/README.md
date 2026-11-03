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
| `birmingham_lsoa.gpkg`, `birmingham_lsoa_table.csv`, `birmingham_msoa.gpkg`, `birmingham_wards.gpkg`, `birmingham_constituencies.gpkg`, `birmingham_dictionary.csv` | ONS Census 2001 and 2011 (via Nomis), 2011 boundaries and lookups; MHCLG English Indices of Deprivation 2019; House of Commons Library 2010 election results. Built by `tools/prepare_birmingham.R` | OGL v3.0 (election results: Open Parliament Licence v3.0) |

### The Birmingham multilevel-modelling data

Four layers on the same 2011 geography, for fitting multilevel models (for example with lme4) and mapping the
results at any level: 639 **LSOAs** (the modelling rows) in 132 **MSOAs** (every LSOA sits wholly inside one), and 40
**wards** and 10 parliamentary **constituencies** (wards are built into constituencies; LSOAs and MSOAs only roughly
nest in wards, so each is given the ward holding most of its residents and MSOA and ward are partly crossed).
`birmingham_lsoa` already carries every variable, with the higher-level ones copied down; the other three layers
hold the same variables for mapping. `birmingham_lsoa_table` is the LSOA layer as a plain CSV (no geometry) to model
without sf; join the results back to a layer on `lsoa11cd` (or `msoa11cd`, `ward11cd`, `pcon_cd`) to map them.
`birmingham_dictionary` explains each column.

- Outcome `y_change`: the area's share of Birmingham's Black African population in 2011 minus its share in 2001 (n/N, in
  percentage points; adds up to 0). `y_change_asinh` is `asinh(y_change / 0.1)`: zero stays zero, the sign is kept and the
  heavy tails are pulled in.
- Predictors: `pct_ba_2001` (percentage Black African, 2001) and `imd_score` (IMD 2019, at LSOA); `msoa_div_*` (ethnic
  diversity, MSOA); `ward_social_*` and `ward_private_*` (tenure, ward); `pcon_labour_2010` and `pcon_turnout_2010`.
- 2001 counts exist for 2001 output areas; they are shared out to 2011 output areas in proportion to 2011 population
  where an area was split (merged areas add up), so a few 2001 counts are not whole numbers.

Not yet included: the Rogers Park residential building outlines (OpenStreetMap, ODbL): they need the
"(c) OpenStreetMap contributors" credit and share-alike notice written up first.
