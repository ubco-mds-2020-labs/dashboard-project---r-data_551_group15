source("src/dataWrangling.R")

# Prepare the input values:
provinces = as.list(Canada_economic_indicators$Geography)
Years = as.list(Canada_economic_indicators$Year)

provinces = unique(provinces)
Years = unique(Years)

geos = list()
for (p in provinces) {
  temp = list(label = p, value = p)
  geos = append(geos, list(temp))
}