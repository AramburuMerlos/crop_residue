host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
	setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/crop_residue")
} else if (host == "LRDAH-DX5B0R3") {
	setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/crop_residue")
}

library(data.table)
library(terra)

# load data ---------
ctries <- c("CHN", "IDN", "IND", "MEX", "MMR", "THA", "VNM")

v <- geodata::gadm(ctries, path = "data")
vd <- as.data.frame(v)
setDT(vd)
vd[, uniqueN(GID_0), by = .(COUNTRY)][V1 >1]
vd[COUNTRY == "China", GID_0:= "CHN"]
vd[COUNTRY == "India", GID_0:= "IND"]

d <- readxl::read_excel("data/subregions/country_subregions_provinces.xlsx")
setDT(d)
d[, COUNTRY:= NULL]

# merge data ------
# check that everything is present
(d[!vd, on = .NATURAL][, .N] == 0) |> stopifnot()

# add missing provinces of India, Indonesia, and elsewhere
dd <- vd[, .(GID_0, NAME_1)][!d, on = .NATURAL]
dd[GID_0 == "CHN" & NAME_1 %in% c("Hainan", "Hong Kong", "Macau"), Region:= "South"]
dd[GID_0 == "IDN", Region:= "Other islands"]
dd[GID_0 == "IND", Region:= "Other Regions"]
dd[NAME_1 == c("Yunnan"), Region:= "Southwest"]
dd[GID_0 == "VNM" & NAME_1 %in% c("Quảng Bình", "Quảng Trị", "Thừa Thiên Huế")
	 , Region:= "Central-South"]
dd[GID_0 == "VNM" & is.na(Region), Region:= "North"]
dd[GID_0 == "MMR" & is.na(Region), Region:= "Other regions"]

d <- rbind(d, dd)

# merge 
vv <- merge(v, d, by = c("GID_0", "NAME_1"), all = FALSE)
vv <- aggregate(vv, c("GID_0", "Region"))

dir.create("maps/country_subregions", F, T)
i=1
fs <- function(x) {
	ex <- ext(x)
	r <- (ex[4] - ex[3])/(ex[2] - ex[1])
	if(r > 1) {
		c(4, 4 * r)
	} else {
		c(4/r, 4)
	}
		
} 
	
col <- palette.colors(7)[-1][c(1,5,3,4,6,2)]

for(i in seq_along(ctries)) {
	vi <- v[v$GID_0 == ctries[i]]
	vvi <- vv[vv$GID_0 == ctries[i]]
	rat <- ext(vvi) 
	png(paste0("maps/country_subregions/", ctries[i], ".png"),
			fs(vvi)[1], fs(vvi)[2], unit = "in", res = 300)
	plot(vvi, "Region", col = col[1:uniqueN(vvi$Region)], legend  = FALSE, las = 1, 
			 mar = rep(1,4), lwd = 1.5, axes = FALSE)
	plot(vi, lwd = 0.5, add = T)
	text(vvi, "Region", halo = T, cex = 0.8, inside = TRUE)
	dev.off()
}

writeVector(vv[, c("GID_0", "Region")], 
						"data/subregions/country_subregions.gpkg", 
						overwrite = T)
dout <- as.data.frame(vv[, c("GID_0", "Region")])
fwrite(dout, "data/subregions/subregions.csv")
