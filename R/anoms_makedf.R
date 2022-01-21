#' Make deficit and surplus data frames
#'
#' @param file_path A file path that points to a .csv file
#' @param pix_size Minimum number of country pixels
#' @param n_coun  Number of countries that will display on a plot
#' @param coun_area Country land area in square kilometers
#' @param min_pop Country population
#'
#' @return A water deficit data frame and water surplus data frame
#' @export
#'
#' @examples
#' file_path <- "~/population_water_anomaly_summary_201801.csv"
#' pix_size <- 10
#' n_coun <- 10
anoms_makedf <- function(file_path, pix_size, coun_area, n_coun, min_pop){

  # read water anomaly .csv as a data frame; ex:population_water_anomaly_summary_200204.csv
  df1 <- readr::read_csv(file_path)
  # remove the world row (last row) from the data frame
  df2 <- utils::head(df1, -1)

  # vector of country abbreviations
  c.abbrev <- c("ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BES","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CUW","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","SSD","STP","SUR","SVK","SVN","SWE","SWZ","SXM","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT","WLF","WSM","XAD","XCA","XCL","XKO","XNC","XPI","XSP","YEM","ZAF","ZMB","ZWE")
  # vector of country areas rounded to the nearest ones
  c.areas <- c(183,644073,1252223,84,1498,28697,451,71359,2780317,29657,224,12265142,7843,437,7702795,83621,86139,27055,30599,115897,325,274478,139704,111398,716,13418,50965,25,206204,22062,68,1087027,8532785,436,5803,38827,77,579354,622869,9904030,14,41137,757301,9380447,323024,468060,2339330,343258,262,1141744,1680,4105,51386,111026,439,140,281,5720,78574,356544,21949,757,42941,48240,2311489,257262,985531,120884,267908,505488,45249,1134221,334795,19026,12350,548106,1431,778,265735,244432,69736,88,239330,7,245806,1656,10711,34015,27080,132465,361,2134445,109535,83670,552,210663,1132,362,112695,56952,27190,92799,1898611,577,3160381,65,70078,1622724,445376,101558,22177,300249,11035,125,89190,372295,2722583,588603,198372,182055,1017,268,100206,17411,230721,10241,96332,1618811,617,162,66117,30582,64719,2573,64392,34,56,413784,2,33815,593213,301,1956408,302,24790,1255277,323,671117,13313,1562132,506,789869,1044152,101,1122,2033,118474,330840,393,825975,18878,1187537,41,912133,128710,267,37519,323082,147881,22,268320,310197,875173,75419,54,1298088,297007,490,465628,311194,8999,122622,91790,400541,6225,4067,11649,2520,237763,16832149,25391,1920472,1878549,197610,700,4079,412,59836,28652,72905,20499,61,636256,226,78072,630779,1007,147072,48948,19931,447345,17415,38,493,186930,994,1274412,57234,515875,142011,16,489169,14978,766,5180,154905,780051,42,36490,944623,242521,599473,47,177563,9456635,446905,1,399,916379,170,363,330372,12361,156,2864,234,371055,10,10902,3314,20,2,453810,1221332,753301,391876)
  # vector of country pixels rounded to the nearest ones
  c.pix <- c(0,251,415,0,1,12,0,25,1113,13,0,24208,4,0,2782,40,37,9,16,38,0,91,49,49,0,5,23,0,112,7,0,368,2837,0,2,14,0,203,203,6772,0,19,317,3808,105,152,760,111,0,371,1,1,17,39,0,0,0,2,39,184,7,0,25,16,851,83,357,41,95,214,28,372,252,6,6,258,1,0,86,135,30,0,78,0,81,1,4,11,9,55,0,2626,37,27,0,68,0,0,38,26,9,44,616,0,1115,0,38,624,172,78,8,133,4,0,34,152,1322,191,86,60,0,0,40,6,79,4,31,589,0,0,22,11,37,1,38,0,0,158,0,16,204,0,695,0,11,426,0,233,6,739,0,268,361,0,0,1,39,107,0,289,7,403,0,300,43,0,20,243,54,0,117,107,327,25,0,427,98,0,152,164,3,52,39,141,2,1,4,1,110,11683,8,682,634,66,0,2,0,100,9,24,7,0,207,0,35,206,0,48,24,9,317,6,0,0,74,0,429,19,173,59,0,204,5,0,2,61,325,0,13,308,79,296,0,68,4468,194,0,0,299,0,0,112,4,0,1,0,161,0,5,1,0,0,153,452,251,134)

  # add country_areas as new column in df2
  df2$country_area <- c.areas
  # add country_abbrev <- new column in df2
  df2$country_abbrev <- c.abbrev
  # add country_pixels as new column in df2
  df2$country_pixels <- c.pix
  # add country_population as new column in df2; sum of columns 3 through 8
  df2$country_population <- rowSums(df2[,3:8])
  # remove countries with less than coun_area sq kilometers and less than min_pop country population
  df3 <- dplyr::filter(df2, country_area >= coun_area & country_population >= min_pop)

  # skinny data frame to focus on countries with exceptional deficit
  dfd <- dplyr::filter(df3, pop_frac_deficit_gt_40 >= 0.02)
  # skinny the defict data frame to n_coun countries with the greatest pop_frac_deficit_gt_40
  dfd <- dfd[order(dfd$pop_frac_deficit_gt_40, decreasing = TRUE), ]
  dfd <- utils::head(dfd,n_coun)
  # change the deficit data frame into a long data frame so that the return periods are categories
  dfdl <- data.frame(country = dfd$country_name,
                     country_area = dfd$country_area,
                     pix_size = dfd$country_pixels,
                     pop_frac = c(dfd$pop_frac_deficit_gt_40,
                                  dfd$pop_frac_deficit_20_40,
                                  dfd$pop_frac_deficit_10_20,
                                  dfd$pop_frac_deficit_5_10,
                                  dfd$pop_frac_deficit_3_5),
                                  #dfd$pop_frac_deficit_lt_3),
                     rp = factor(c(rep("> 40", nrow(dfd)),
                                   rep("20-40", nrow(dfd)),
                                   rep("10-20", nrow(dfd)),
                                   rep("5-10", nrow(dfd)),
                                   rep("3-5", nrow(dfd))),
                                   #rep("< 3", nrow(dfd))),
                                 levels = c("3-5", "5-10", "10-20", "20-40", "> 40"))
                    )
  # make countries a factor so it doesn't plot alphabetically
  dfdl$country <- factor(dfdl$country, levels = rev(unique(dfdl$country)))


  # skinny data frame to focus on countries with exceptional surplus
  dfs <- dplyr::filter(df3, pop_frac_surplus_gt_40 >= 0.01)
  # skinny the surplus data frame to n_coun countries with the greatest pop_frac_surplus_gt_40
  dfs <- dfs[order(dfs$pop_frac_surplus_gt_40, decreasing = TRUE), ]
  dfs <- utils::head(dfs,n_coun)
  # change the surplus data frame into a long data frame so that the return periods are categories
  dfsl <- data.frame(country = dfs$country_name,
                     country_area = dfs$country_area,
                     pix_size = dfs$country_pixels,
                     pop_frac = c(dfs$pop_frac_surplus_gt_40,
                                  dfs$pop_frac_surplus_20_40,
                                  dfs$pop_frac_surplus_10_20,
                                  dfs$pop_frac_surplus_5_10,
                                  dfs$pop_frac_surplus_3_5),
                                  #dfs$pop_frac_surplus_lt_3),
                     rp = factor(c(rep("> 40", nrow(dfs)),
                                   rep("20-40", nrow(dfs)),
                                   rep("10-20", nrow(dfs)),
                                   rep("5-10", nrow(dfs)),
                                   rep("3-5", nrow(dfs))),
                                   #rep("< 3", nrow(dfs))),
                                 levels = c("3-5", "5-10", "10-20", "20-40", "> 40"))
  )
  # make countries a factor so it doesn't plot alphabetically
  dfsl$country <- factor(dfsl$country, levels = rev(unique(dfsl$country)))

df_list <- list(dfdl, dfsl)

return(df_list)

}
