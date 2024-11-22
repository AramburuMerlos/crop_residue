host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
	setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/crop_residue")
} else if (host == "LRDAH-DX5B0R3") {
	setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/crop_residue")
}

library(data.table)
library(stringr)


# cropland nutrient database -----------
# load complete database
dt <- fread(
	"data/faostat/cropland_nutrient_database/Environment_Cropland_nutrient_budget_E_All_Data.csv"
	, encoding = "Latin-1"
)
setnames(dt, names(dt), tolower(str_replace(names(dt), " ", "_")))
dt <- dt[, .(area_code, area)] |> unique()
# split countries and regions
# send China to regions and keep the 4 subregions (mainland, Macau, Hong Kong, Taiwan) in countries
countries <- dt[area_code < 5000 & area != "China"]
regions <- dt[area_code >= 5000 | area == "China"]
# add iso3 and other info from geodata package
cc <- geodata::country_codes()
setDT(cc)
setnames(cc, names(cc), tolower(names(cc)))
# fix fao names in geodata
cc[name == "Micronesia", name_fao:= "Micronesia (Federated States of)"]
cc[name == "Nauru", name_fao:= "Nauru"]
cc[name == "Netherlands", name_fao:= "Netherlands (Kingdom of the)"]

# join databases
countries <- cc[, .(iso3, name_fao, unregion1, unregion2)][
	countries, on = .(name_fao = area)]

# check completeness
stopifnot(all(!is.na(countries$iso3)))

# crop and livestock production database ------------------
## crop groups ----
fn <- list.files("data/faostat/crop_groups", full.names = TRUE)
lg <- lapply(fn, fread)
names(lg) <- basename(fn) |> str_remove(".csv")
dg <- rbindlist(lg, id = "crop_group")
dg <- dg[, .(crop_group, crop = Item, cpc_code = `Item Code (CPC)`)] |>
	unique()

## add old fao item code
di <- fread(
	"data/faostat/crops_and_livestock_products/Production_Crops_Livestock_E_ItemCodes.csv", 
	encoding = "Latin-1"
)
setnames(di, c("Item", "Item Code"), c("crop", "crop_code"))
# fix crop names so they match
di[, crop:= str_replace_all(crop, ";", ",")]
# join table by names
dg <- di[dg, on = .(crop)]
# check consistency in CPC code
dg[, all(cpc_code == as.numeric(str_remove(`CPC Code`, "\\'")))] |>
	stopifnot()
# keep the CPC code with ', as this is how it is used in the other dataset
dg[, cpc_code:= NULL]
setnames(dg, "CPC Code", "cpc_code")

rm(di)

## Crop Country Area by year --------------
d <- fread(
	"data/faostat/crops_and_livestock_products/Production_Crops_Livestock_E_All_Data_NOFLAG.csv", 
)
# rename variables
setnames(d, names(d), tolower(str_replace_all(names(d), " ", "_")))
setnames(d, names(d), str_replace(names(d), "item", "crop"))
setnames(d, names(d), str_remove_all(names(d), "\\(|\\)"))
setnames(d, "crop_code_cpc", "cpc_code")
setnames(d, "area", "name_fao")
# subset crop variables
d <- d[element %in% c("Area harvested", "Production", "Yield")]
# subset crops
d <- d[dg, on = .NATURAL]
# are all crops present?
all(d$crop %in% dg$crop) |> stopifnot()
# subset countries
d <- d[countries, on = .NATURAL]
# are all countries present
all(d$name_fao %in% countries$name_fao) |> stopifnot()

# reshape to long
d <- melt(d, measure.vars = patterns("^y\\d{4}"), variable.name = "year")
# check units
d[, .N, by = .(element, unit)]
# rename elements (future column variables with values)
d[element == "Area harvested", element:= "area_ha"]
d[element == "Production", element:= "prod_t"]
d[element == "Yield", element:= "yield_kg_ha"]
d[, unit:= NULL]
d[, element_code:= NULL]
# reshape to wide (area, yield, and prod in diff columns)
d <- dcast(d, ... ~ element)
d[, year:= as.integer(str_remove(year, "y"))]

setnames(d, "name_fao", "country")
setcolorder(d, c("iso3", "unregion1", "unregion2"), before = "crop_code")
fwrite(d, "data/faostat/prod_&_area_all_crop_country_years.csv")

# Rank crops ----------------
# rank crops based on current (last 5 years) area
dd <- d[year > 2017, .(area_ha = mean(area_ha, na.rm = TRUE)), by = key(d)]
setcolorder(dd, c("iso3", "unregion1", "unregion2"), before = "crop_code")
dd[, area_code_m49:= NULL]
dd[, cpc_code:= NULL]
setcolorder(dd, c("unregion2", "unregion1", "country"))
setcolorder(dd, "crop", before = "crop_code")
setcolorder(dd, "crop_group", before = "crop")
dd[, crop_rank:= frankv(area_ha, order = -1, na.last = "keep", ties.method = "random"), 
	 by = .(country)]
setorder(dd, unregion2, unregion1, country, crop_rank, na.last = TRUE)
dd

fwrite(dd, "data/faostat/current_area_all_crop_country_ranked.csv")

# Country classification ------------
# decide whether a country is surveyed individually, by region, or extrapolated
# country area totals
dc <- dd[, .(area_ha = sum(area_ha, na.rm = TRUE))
				 , by = .(unregion2, unregion1, country, area_code, iso3)
]
# countries that will be analyzed by region
dc[unregion2 %in% c("Africa", "Eurpope"), area_unit:= "unregion1"]



dc[unregion2 == "Oceania", .N, by = .(unregion1)]

dc[unregion2 %in% c("Africa", "Eurpope"), area_unit:= "unregion1"]
