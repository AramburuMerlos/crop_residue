host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
	setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/crop_residue")
} else if (host == "LRDAH-DX5B0R3") {
	setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/crop_residue")
}

library(data.table)
library(stringr)
library(terra)

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
setnames(cc, c("unregion2", "unregion1"), c("continent", "region"))

# fix fao names in geodata
cc[name == "Micronesia", name_fao:= "Micronesia (Federated States of)"]
cc[name == "Nauru", name_fao:= "Nauru"]
cc[name == "Netherlands", name_fao:= "Netherlands (Kingdom of the)"]

# join databases
countries <- cc[, .(iso3, name_fao, region, continent)][
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
setcolorder(d, c("iso3", "region", "continent"), before = "crop_code")
fwrite(d, "data/faostat/prod_&_area_all_crop_country_years.csv")

# Rank crops ----------------
# rank crops based on current (last 5 years) area
dd <- d[year > 2017, .(area_ha = mean(area_ha, na.rm = TRUE)), by = key(d)]
setcolorder(dd, c("iso3", "region", "continent"), before = "crop_code")
dd[, area_code_m49:= NULL]
dd[, cpc_code:= NULL]
setcolorder(dd, c("continent", "region", "country"))
setcolorder(dd, "crop", before = "crop_code")
setcolorder(dd, "crop_group", before = "crop")
dd[, crop_rank:= frankv(area_ha, order = -1, na.last = "keep", ties.method = "random"), 
	 by = .(country)]
setorder(dd, continent, region, country, crop_rank, na.last = TRUE)
dd

fwrite(dd, "data/faostat/current_area_all_crop_country_ranked.csv")
dd[, crop_rank:= NULL]


# Survey units ------------
# calculate country total cropland area
dc <- dd[, .(cropland_Mha = sum(area_ha, na.rm = TRUE)/1e6)
				 , by = .(continent, region, country, area_code, iso3)
]

## by UN region ---------
dc[continent %in% c("Africa", "Europe", "Oceania"), survey_unit:= region]
dc[region %in% c("Western Asia", "Central Asia", "Central America"), survey_unit:= region]

## by custom regions ----------
dc[iso3 %in% c("USA", "CAN"), survey_unit:= "USA - Canada"]
dc[iso3 %in% c("ARG", "BRA", "PRY", "URY"), survey_unit:= "Mercosur"]
dc[region == "South America" & is.na(survey_unit), survey_unit:= "Andean counries"]


## by country  ------------
ind_ctry <- c(
	"IND", "CHN", "MEX", "PHL", "IDN", "AUS" 
)
dc[iso3 %in% ind_ctry, survey_unit:= country]

## by region minus important country -----------
dc[is.na(survey_unit), survey_unit:= paste(region, "(others)")]

## extrapolated ------------
# leave blank
dc[region %in% c("Caribbean", "Melanesia", "Polynesia", "Micronesia"), survey_unit:= NA]
# north korea, guyana, suriname
dc[iso3 %in% c("PRK", "GUY", "SUR"), survey_unit:= NA]
# small country/islands/deserts with small crop area (less than 700k ha)
dc[cropland_Mha < 0.7, survey_unit:= NA]

dc[!is.na(survey_unit), uniqueN(survey_unit)]
dc[, .N, by = .(survey_unit)]
setorder(dc, continent, region, country)

## save as excel ---------
dt1 <- dc[, .(continent, region, country, survey_unit, iso3, `cropland (Mha)` = round(cropland_Mha, 1))]
dt2 <- dc[!is.na(survey_unit), .(countries = paste(country, collapse = ", ")), by = .(survey_unit)]
dir.create("data/excel_files", F, T)
writexl::write_xlsx(
	list(country_list = dt1, survey_unit_list = dt2), 
	path = "data/excel_files/survey_unit.xlsx"
)

rm(dt1, dt2)

# Crop per survey unit ------------------------
dt <- dd[dc[!is.na(survey_unit)], on = .NATURAL]
dtt <- dt[, .(cropland_ha = sum(area_ha, na.rm = T)), by = .(survey_unit)]
dt <- dt[
	, .(area_ha = sum(area_ha, na.rm = TRUE))
	, by = .(survey_unit, crop_group, crop, crop_code)
]
dt <- dt[dtt, on = .(survey_unit)]
setorderv(dt, c("survey_unit", "area_ha"), order = c(1,-1), na.last = TRUE)
dt[, crop_prop:= area_ha/(cropland_ha)]
dt[, crop_cum_prop:= cumsum(crop_prop), by = .(survey_unit)]
dt[, max(crop_cum_prop), by = .(survey_unit)][, all.equal(V1, rep(1, .N))] |> 
	stopifnot()
dt[crop_cum_prop < 0.7,]

dt1 <- dt[, head(.SD), by = .(survey_unit, crop_group)]


# map --------------
w <- geodata::world(path = "data")
w0 <- merge(w, dc, by.x = "GID_0", by.y = "iso3")
w1 <- aggregate(w0, "survey_unit")
w1 <- w1[order(w1$continent)]

set.seed(10)
colors <- paletteer::paletteer_d("ggthemes::Tableau_20", 20) |>
	c("red", "blue", "green", "yellow") |> sample()
dir.create("maps", F)

png("maps/survey_units.png", width = 7, height = 5, res = 300, units = "in")
plot(w1, "survey_unit", lwd = 1.5, col = colors, legend = FALSE, mar = c(7,1,1,1), sort = FALSE)
plot(w, add = T, lwd = 0.7)
legend(x = "bottom", 
			legend = c(w1$survey_unit[!is.na(w1$survey_unit)], "extrapolated"), 
			 ncol = 4, fill = c(colors, "white"), xpd = NA, inset = -0.4, cex = 0.7)
dev.off()

