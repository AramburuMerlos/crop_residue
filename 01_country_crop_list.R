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

# keep group, name and code only
dg <- dg[, .(crop_group, crop = Item, cpc_code = `Item Code (CPC)`)] |>
	unique()

# remove cotton from oil crops
dg <- dg[!(crop_group == "Oilcrops" & crop == "Seed cotton, unginned")]

### Split Cereals ----------------------
dg[crop_group == "Cereals"]
dg[crop %in% c("Maize (corn)", "Millet", "Sorghum"), crop_group:= "Coarse grains"]
dg[crop %in% c("Barley", "Rye", "Triticale", "Wheat", "Oats"), crop_group:= "Small grains"]
dg[crop == "Rice", crop_group:= "Rice"]
dg[crop_group == "Cereals", crop_group:= "Other Cereals"]

### Tee crops ---------------
# let's define a tree crop category
tree_crops <- c("Apples", "Apricots", "Avocados", "Cashewapple", "Cherries", "Dates", "Figs", 
								"Grapes", "Kiwi fruit", "Lemons and limes", "Locust beans (carobs)", 
								"Mangoes, guavas and mangosteens", "Olives", "Oranges", 
								"Other citrus fruit, n.e.c.", "Other pome fruits", "Other stone fruits", 
								"Other tropical fruits, n.e.c.", "Papayas", "Peaches and nectarines", 
								"Pears", "Persimmons", "Plums and sloes", "Pomelos and grapefruits", 
								"Quinces", "Sour cherries", "Tangerines, mandarins, clementines", 
								"Coconuts, in shell", "Karite nuts (sheanuts)", "Oil palm fruit", 
								"Tallowtree seeds", "Tung nuts")


dg[crop_group == "Treenuts" | crop %in% tree_crops, crop_group:= "Tree Crops"]

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
# select crop variables (rm livestock variables)
d <- d[element %in% c("Area harvested", "Production", "Yield")]
# select crops (rm livestock rows)
d <- d[dg, on = .NATURAL]
# are all crops present?
all(d$crop %in% dg$crop) |> stopifnot()
# subset countries (to keep the ones in the cropland nutrient database)
d <- d[countries, on = .NATURAL]
# are all countries present?
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
# save data
setnames(d, "name_fao", "country")
setcolorder(d, c("iso3", "region", "continent"), before = "crop_code")
fwrite(d, "data/faostat/prod_&_area_all_crop_country_years.csv")

# Current yield/area ----------------
#  last 5 years average
dd <- d[year > 2017, .(area_ha = mean(area_ha, na.rm = TRUE)), by = key(d)]
setcolorder(dd, c("iso3", "region", "continent"), before = "crop_code")
# remove unnecessary columns
dd[, area_code_m49:= NULL]
dd[, cpc_code:= NULL]
setcolorder(dd, c("continent", "region", "country"))
setcolorder(dd, "crop", before = "crop_code")
setcolorder(dd, "crop_group", before = "crop")
# add crop rank
dd[, crop_rank:= frankv(area_ha, order = -1, na.last = "keep", ties.method = "random"), 
	 by = .(country)]
setorder(dd, continent, region, country, crop_rank, na.last = TRUE)
dd
# save data
fwrite(dd, "data/faostat/current_area_all_crop_country_ranked.csv")
dd[, crop_rank:= NULL]

# calc country total cropland area ------------
dc <- dd[, .(cropland_Mha = sum(area_ha, na.rm = TRUE)/1e6)
				 , by = .(continent, region, country, area_code, iso3)
]

# Survey units ------------
# define survey units for google forms


## by custom regions ----------
dc[iso3 %in% c("USA", "CAN"), survey_unit:= "USA - Canada"]
dc[iso3 %in% c("AUS", "NZL"), survey_unit:= "Australia - New Zeland"]
dc[iso3 %in% c("ARG", "BRA", "PRY", "URY"), survey_unit:= "Mercosur"]
dc[region == "South America" & is.na(survey_unit), survey_unit:= "Andean counries"]


## by country  ------------
# China is the only country that will be surveyed individually
dc[iso3 == "CHN", survey_unit:= country]
# all other countries in E Asia go together
dc[region == "Eastern Asia" & is.na(survey_unit), survey_unit:= "Eastern Asia (others)"]

## by region -----------
# all others go by region
dc[is.na(survey_unit), survey_unit:= region]

## extrapolated ------------
# countries without survey unit (blank - NA) will be extrapolated
# islands
dc[region %in% c("Caribbean", "Melanesia", "Polynesia", "Micronesia"), survey_unit:= NA]
# north korea, guyana, suriname, Taiwan
dc[iso3 %in% c("PRK", "GUY", "SUR", "TWN"), survey_unit:= NA]
# all countries with small crop area (less than 700k ha)
dc[cropland_Mha < 0.4 & iso3 != "NZL", survey_unit:= NA]

dc[!is.na(survey_unit), uniqueN(survey_unit)]
dc[, .N, by = .(survey_unit)]
setorder(dc, continent, region, country)


# Crop per survey unit ------------------------
# identify the most important crop per survey unit
## calc totals and prop ----------
# add crops to survy unites data
dt <- dd[dc[!is.na(survey_unit)], on = .NATURAL]
# total cropland area per survey unit
dtt <- dt[, .(cropland_ha = sum(area_ha, na.rm = T)), by = .(survey_unit)]
# total crop area per crop and survey unit
dt <- dt[
	, .(area_ha = sum(area_ha, na.rm = TRUE))
	, by = .(survey_unit, crop_group, crop, crop_code)
]
# add survey unit cropland totals
dt <- dt[dtt, on = .(survey_unit)]
# order data by survey unit and crop area 
setorderv(dt, c("survey_unit", "area_ha"), order = c(1,-1), na.last = TRUE)
# calc crop proportion in survey unit and cumulative proportion
dt[, crop_prop:= area_ha/(cropland_ha)]
dt[, crop_cum_prop:= cumsum(crop_prop), by = .(survey_unit)]
# check that the maximum of the cumulative proportion is always 1
dt[, max(crop_cum_prop), by = .(survey_unit)][, all.equal(V1, rep(1, .N))] |> 
	stopifnot()


## select most important crops ------------
# select crops to cover 75% of the cropland area
rbind(
	dt[crop_cum_prop < 0.75, ] ,
	dt[crop_cum_prop > 0.75, .SD[1], by = .(survey_unit)]
)[order(survey_unit)] |>
	unique() -> ds

# if the most important crop in a group not yet included covers > 1%, include it
rbind(
	ds, 
	dt[!ds, on = .(survey_unit, crop_group)][
		crop_prop > 0.011, .SD[1], by = .(survey_unit, crop_group)
	]			
) -> ds


setorderv(ds, c("survey_unit", "crop_prop"), order = c(1,-1))

## crops in survey? -------------
# define whether they will be surveyed or can be inferred from other info
ds[, survey_crop:= TRUE]

### no tree crops, fruits and vegetables -------
ds[crop_group %in% c("Tree Crops", "Fruit", "Vegetables"), survey_crop:= FALSE]

# no "Other" or n.e.c crop
ds[crop %like% "Other|n\\.e\\.c\\.", survey_crop:= FALSE]

### similar crops  ----------
# keep the most important (first appearence, since they are in order) from the following groups
# Coarse cereal grains
ds[crop_group == "Coarse grains"
	 , survey_crop:= c(TRUE, rep(FALSE, .N - 1))
	 , by = .(survey_unit)
]
	
# winter C3 crops 
ds[crop_group == "Small grains"
	 , survey_crop:= c(TRUE, rep(FALSE, .N - 1))
	 , by = .(survey_unit)
]

# root and tuber crops
ds[crop_group == "Root and Tubers"
		, survey_crop:= c(TRUE, rep(FALSE, .N - 1))
		, by = .(survey_unit)
]

# Fibre
ds[crop_group == "Fibre Crops"
		, survey_crop:= c(TRUE, rep(FALSE, .N - 1))
		, by = .(survey_unit)
]

# Pulses
ds[crop_group == "Pulses"
		, survey_crop:= c(TRUE, rep(FALSE, .N - 1))
		, by = .(survey_unit)
]

# Oil crops
# only remove when one is present and the second one is less than 3%
ds[crop_group == "Oilcrops", 
	 survey_crop:= ifelse(crop_prop > 0.03 | c(TRUE, rep(FALSE, .N-1)), TRUE, FALSE)]


# total number of crops to be survey by group
ds[, sum(survey_crop), by = .(survey_unit)]

## Modify names of crops in survey ----------
ds[, crop_sname:= tolower(crop)]
ds[, crop_sname:= str_remove_all(crop_sname, "(?<!lin) ?seed ?")]
ds[crop_group == "Coarse grains", unique(crop)]
ds[crop_group == "Coarse grains", crop_sname:= "coarse grains (maize/corn, sorghum, millet)"]
ds[crop_group == "Small grains", unique(crop)]
ds[crop_group == "Small grains", crop_sname:= "small grains (wheat, barley, teff)"]
ds[crop_group == "Pulses", unique(crop)]
ds[crop_group == "Pulses", crop_sname:= "pulses (beans, cowpeas, chickpeas, etc)"]
ds[crop_group == "Root and Tubers", unique(crop)]
ds[crop_group == "Root and Tubers", crop_sname:= "root and tubers (cassava, potatoes, yams, etc)"]
ds[crop_sname == "soya beans", crop_sname:= "soya bean/soybean"]
ds[crop_sname == "rape or colza", crop_sname:= "rapeseed/colza/mustard"]
ds[crop_sname == "cotton, unginned", crop_sname:= "cotton"]
ds[crop_sname == "groundnuts, excluding shelled", crop_sname:= "groundnut/peanut"]

ds[survey_crop == TRUE, unique(crop_sname)]

# Save as excel ---------
## sheet with country/subregion list ---------
dc1 <- dc[, .(continent, region, country, survey_unit, iso3)]

## Add country subregions -------------------------------
sub_regions <- fread("data/subregions/subregions.csv")
setnames(sub_regions, "Region", "subregion")
dc1 <- merge(dc1, sub_regions, by.x = "iso3", by.y = "GID_0", all.x = T)
setcolorder(dc1, c("continent", "region", "iso3",  "country", "subregion", "survey_unit"))
dc1[country == "Thailand" & subregion == "South", survey_unit:= NA]

# sheet with crop list
ds1 <- ds[, .(survey_unit, crop, crop_sname, crop_group, 
							`area (%)` = round(crop_prop*100), 
							`survey?` = ifelse(survey_crop, "yes", "no"))]

# crop and country by sampling unit list
dc2 <- dc[!is.na(survey_unit), .(countries = paste(country, collapse = ", ")), by = .(survey_unit)]
ds2 <- ds[survey_crop == TRUE, .(crops = paste(crop_sname, collapse = "; ")), by = .(survey_unit)]

dcs <- dc2[ds2, on = .(survey_unit)]

# save excel
writexl::write_xlsx(
	list(summary = dcs, country_list = dc1, crop_list = ds1), 
	path = "data/excel_files/survey_unit_countries_and_crop_lists.xlsx"
)

rm(ds1, ds2)



# maps --------------
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

