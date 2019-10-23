# madison_bike_counts
Analyzing data from bike counters in Madison (WI)

## Climate data
Climate data comes from National Climatic Data Center (NCDC) via the `rnoaa` package. 

`datatype` column:
  `PRCP` = amount of precipitation (unit: 0.1 mm)
  `SNOW` = amount of snowfall (unit: 1 mm)
  `SNWD` = snow depth (unit: 1 mm)
  `TMAX` = maximum daily temperature (unit: 0.1 °C)
  `TMIN` = minimum daily temperature (unit: 0.1 °C)
  `TOBS` = temperature at time of observation (unit: 0.1 °C)

`fl_m` = field to indicate trace amount of PRCP or SNOW

`fl_q` = quality control indicator

`fl_so` = unclear. Most values are 7, but there are also H and Z` 

`fl_t` = time of observation
