test_that("benchmark_list check", {
  expect_type(infrasap::benchmark_list, "character")
  
  expect_equal(infrasap::benchmark_list, c("East Asia & Pacific", "Europe & Central Asia", "Fragile", 
                              "High income", "Isolated", "Latin America & Caribbean", "Low Human Capital", 
                              "Low income", "Low Population Density", "Lower middle income", 
                              "Middle East & North Africa", "Mountainous", "North America", 
                              "OECD members", "Oil Exporter", "South Asia", "Sub-Saharan Africa", 
                              "Upper middle income"))  
})


test_that("dat_bm check", {
  expect_s3_class(infrasap::dat_bm, "data.frame")
  
  expect_equal(names(infrasap::dat_bm),
  c("Grouping", "Indicator", "Sector", "1990", "1991", "1992", 
    "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", 
    "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
    "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
    "2017", "2018", "2019", "2020", "Mode"))
})


test_that("dat_country_income_region check", {
  expect_s3_class(infrasap::dat_country_income_region, "data.frame")
  
  expect_equal(names(infrasap::dat_country_income_region),c("Country Name", "IncomeGroup",  "Region"))
})

test_that("dat_layout check", {
  expect_type(infrasap::dat_layout, "list")
  
  expect_equal(names(infrasap::dat_layout),
        c("energy__governance", "transport__governance", "digital__governance", 
          "energy__connectivity", "digital__connectivity", "energy__finance", "transport__finance"))
})

test_that("dat_ports_bm check", {
  expect_s3_class(infrasap::dat_ports_bm, "data.frame")
  
  expect_equal(names(infrasap::dat_ports_bm),
               c("Region", "Indicator_Name", "Sector", "1990", "1991", "1992", 
                 "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", 
                 "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
                 "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                 "2017", "2018", "2019", "2020"))
})

test_that("dat_ports check", {
  expect_s3_class(infrasap::dat_ports, "data.frame")
  
  expect_equal(names(infrasap::dat_ports),
               c("Country Name", "Country Code", "Sub-national Unit", "Sub-national Unit Name", 
                 "Country_Region", "Port Size", "Indicator Name", "Distribution1 Condition", 
                 "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", 
                 "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", 
                 "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", 
                 "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", 
                 "2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", 
                 "2030", "Match1", "Match2"))
})


test_that("dat check", {
  expect_s3_class(infrasap::dat, "data.frame")
  
  expect_equal(names(infrasap::dat),
               c("Country Name", "Country Code", "Region", "OECD Member", "IncomeGroup", 
                 "Isolated", "Mountainous", "Low Population Density", "Oil Exporter", 
                 "Human Capital", "Fragile", "Indicator Name", "Indicator Sector", 
                 "Indicator Pillar", "Mode", "Indicator Sub-Pillar", "Indicator Topic", 
                 "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", 
                 "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", 
                 "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", 
                 "2014", "2015", "2016", "2017", "2018", "2019", "2020", "Region Benchmark Achieved 1990", 
                 "irf_data", "Type of Benchmark"))
})



test_that("irf_indicators check", {
  expect_type(infrasap::irf_indicators, "character")
  
  expect_equal(infrasap::irf_indicators,
               c("Fuel Price: Diesel price (USD/litre)", "Fuel Price: Super Gasoline price (USD/litre)", 
                 "Intensity: Intensity of use of buses (Mil veh-km/veh)", "Intensity: Intensity of use of cars (Mil veh-km/veh)", 
                 "Intensity: Intensity of use of motocycles (Mil veh-km/veh)", 
                 "Intensity: Intensity of use of primary road network (Mil veh-km/km)", 
                 "Intensity: Intensity of use of secondary road network (Mil veh-km/km)", 
                 "Intensity: Intensity of use of tertiary road network (Mil veh-km/km)", 
                 "Intensity: Intensity of use of vans,pickups,lorries,tractors(Mil veh-km/veh)", 
                 "Multimodal: % of inland surface Frght transport carried by inlandwaterway", 
                 "Multimodal: % of inland surface Frght transport carried by rail", 
                 "Multimodal: % of inland surface Frght transport carried by road", 
                 "Multimodal: % of inland surface Psngr private transport on road", 
                 "Multimodal: % of inland surface Psngr public transport on road", 
                 "Multimodal: % of inland surface Psngr transport carried by rail", 
                 "Multimodal: % of inland surface Psngr transport carried by road", 
                 "Multimodal: 5-year CAGR: Inland surface  passenger transport", 
                 "Multimodal: 5-year CAGR: Inland surface freight transport Total", 
                 "Multimodal: Inland surface  passenger transport Total (Mio Passenger-Km)", 
                 "Multimodal: Inland surface freight transport Total (Mio Tonne-Km)", 
                 "Road Networks: % of primary network that is classified as a motorway/highway", 
                 "Road Networks: % of total road network that is paved (%)", "Road Networks: Length of Total length of road network (km)", 
                 "Road Networks: Road network density over the surface (km roads/km²land area)", 
                 "Road Safety: % of road fatalities that are cyclists", "Road Safety: % of road fatalities that are drivers of four-wheeled veh", 
                 "Road Safety: % of road fatalities that are motorized two or three-wheelers", 
                 "Road Safety: % of road fatalities that are passengers of four-wheeled veh", 
                 "Road Safety: % of road fatalities that are pedestrians", "Road Safety: Fatalities / 100'000 population (latest year)", 
                 "Road Safety: Injury accidents / 100'000 population (latest year)", 
                 "Road Traffic: % of road traffic in buses and coaches", "Road Traffic: % of road traffic in cars", 
                 "Road Traffic: % of road traffic in motorcycles", "Road Traffic: % of total road traffic in vans,pickups,lorries,tractors", 
                 "Road Traffic: Total volume of passenger road traffic (Mil veh-km,Annual)", 
                 "Road Traffic: Total volume of vans,pickups,lorries,tractors(Mil veh-km,Annual)", 
                 "Vehicle in use: % of total vehicles that are buses and motor coaches", 
                 "Vehicle in use: % of total vehicles that are cars", "Vehicle in use: % of total vehicles that are commercial vehicles", 
                 "Vehicle in use: % of total vehicles that are motorcycles", "Vehicle in use: Penetration of buses (veh/’000 pop)", 
                 "Vehicle in use: Penetration of cars (veh/’000 pop)", "Vehicle in use: Penetration of commercial vehicles (veh/’000 pop)", 
                 "Vehicle in use: Penetration of motocycles (veh/’000 pop)", 
                 "Vehicles in use: total number of commercial veh(van,pickup,lorry,tractor)", 
                 "Vehicles in use: Total number of motorized passenger vehicles"
               ))
})


test_that("map_location check", {
  expect_s3_class(infrasap::map_location, "data.frame")

  expect_equal(names(infrasap::map_location),c("region", "lat", "lon", "zoom"))
})


test_that("scd_bm check", {
  expect_s3_class(infrasap::scd_bm, "data.frame")
  
  expect_equal(names(infrasap::scd_bm),c("Grouping", "Indicator", "Sector", "1990", "1991", "1992", 
                                         "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", 
                                         "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
                                         "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                                         "2017", "2018", "2019", "2020", "Mode", "new_name", "grouping"
  ))
})

test_that("scd_dat_countries_with_article check", {
  expect_type(infrasap::scd_dat_countries_with_article, "character")
  
  expect_equal(infrasap::scd_dat_countries_with_article,c("United Arab Emirates", "Bahamas, The", "Central African Republic", 
                                                         "Comoros", "Czech Republic", "Dominican Republic", "United Kingdom", 
                                                         "Gambia, The", "Maldives", "Marshall Islands", "Netherlands", 
                                                         "Philippines", "Solomon Islands", "United States", "Channel Islands", 
                                                         "Cayman Islands", "Isle of Man", "Turks and Caicos Islands", 
                                                         "Virgin Islands (U.S.)", "Falkland Islands"))
})


test_that("scd_dat check", {
  expect_s3_class(infrasap::scd_dat, "data.frame")
  
  expect_equal(names(infrasap::scd_dat),c("Country Name", "Country Code", "Region", "OECD Member", "IncomeGroup", 
                                   "Isolated", "Mountainous", "Low Population Density", "Oil Exporter", 
                                   "Human Capital", "Fragile", "Indicator Name", "Indicator Sector", 
                                   "Indicator Pillar", "Mode", "Indicator Sub-Pillar", "Indicator Topic", 
                                   "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", 
                                   "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", 
                                   "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", 
                                   "2014", "2015", "2016", "2017", "2018", "2019", "2020", "Region Benchmark Achieved 1990", 
                                   "irf_data", "Type of Benchmark", "new_name", "grouping"))
})


test_that("scd_indicators check", {
  expect_s3_class(infrasap::scd_indicators, "data.frame")
  
  expect_equal(names(infrasap::scd_indicators),c("Indicator Name","grouping"))
})


test_that("world check", {
  expect_s4_class(infrasap::world, "SpatialPolygonsDataFrame")
  
  expect_equal(names(infrasap::world),c("ISO_A3", "ISO_A2", "WB_A3", "HASC_0", "NAM_0"))
})
