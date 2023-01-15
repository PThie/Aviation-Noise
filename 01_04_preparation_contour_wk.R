################################################################
# Load Data                                                    #
################################################################

# load WK data
red <- qs::qread(
    file.path(
        data_path, "housing/WK_prepared.qs"
    )
)

# geo info
krs <- st_read(file.path(dataGebiete, "Kreis/2019/VG250_KRS.shp"))
gem <- st_read(file.path(dataGebiete, "Gemeinde/2019/VG250_GEM.shp"))

################################################################
# Preparation                                                  #
################################################################

# prepare contour ---------------------------------------------------------

# add ICAO to additional airports
ball_contour$ICAO <- NA
ball_contour$ICAO[ball_contour$Agglomerat == "Essen"] <- "EDLE"
ball_contour$ICAO[ball_contour$Agglomerat == "Mülheim an der Ruhr"] <- "EDLE"
ball_contour$ICAO[ball_contour$Agglomerat == "Mannheim"] <- "EDFM"
ball_contour$ICAO[ball_contour$Agglomerat == "Dortmund"] <- "EDLW"
ball_contour$ICAO[ball_contour$Agglomerat == "Bremen"] <- "EDDW"
ball_contour$ICAO[ball_contour$Agglomerat == "Mainz"] <- "EDFZ"
ball_contour$ICAO[ball_contour$Agglomerat == "Dresden"] <- "EDDC"

ball_contour <- ball_contour[, c("ICAO", "DB_Low", "DB_High", "geometry")]

# identifier for main airports
haupt_contour$main_airport <- 1
ball_contour$main_airport <- 0

# combine both airport "types"
airport_contour <- rbind(haupt_contour, ball_contour)


# construct time variables ---------------------------------------------
red$fir_lockdown <- 0
red$fir_lockdown[red$year_mon >= "2020-03"] <- 1

# January to March 2018
red$janmar18 <- 0
red$janmar18[red$year_mon >= "2018-01" & red$year_mon <= "2018-03"] <- 1

# April to June 2018
red$aprjun18 <- 0
red$aprjun18[red$year_mon >= "2018-04" & red$year_mon <= "2018-06"] <- 1

# July to September 2018
red$julsep18 <- 0
red$julsep18[red$year_mon >= "2018-07" & red$year_mon <= "2018-09"] <- 1

# October to December 2018
red$octdec18 <- 0
red$octdec18[red$year_mon >= "2018-10" & red$year_mon <= "2018-12"] <- 1

# January to March 2019
red$janmar19 <- 0
red$janmar19[red$year_mon >= "2019-01" & red$year_mon <= "2019-03"] <- 1

# April to June 2019
red$aprjun19 <- 0
red$aprjun19[red$year_mon >= "2019-04" & red$year_mon <= "2019-06"] <- 1

# July to September 2019
red$julsep19 <- 0
red$julsep19[red$year_mon >= "2019-07" & red$year_mon <= "2019-09"] <- 1

# October to December 2019
red$octdec19 <- 0
red$octdec19[red$year_mon >= "2019-10" & red$year_mon <= "2019-12"] <- 1

# January to February 2020
red$janfeb <- 0
red$janfeb[red$year_mon >= "2020-01" & red$year_mon <= "2020-02"] <- 1

# March to December 2020
red$mardec <- 0
red$mardec[red$year_mon >= "2020-03"] <- 1

# March to May 2020
red$marmay <- 0
red$marmay[red$year_mon >= "2020-03" & red$year_mon <= "2020-05"] <- 1

# June to September 2020
red$junsep <- 0
red$junsep[red$year_mon >= "2020-06" & red$year_mon <= "2020-09"] <- 1

# October to December 2020
red$octdec <- 0
red$octdec[red$year_mon >= "2020-10" & red$year_mon <= "2020-12"] <- 1

# January to March 2021
red$janmar21 <- 0
red$janmar21[red$year_mon >= "2021-01" & red$year_mon <= "2021-03"] <- 1

# April to June 2021
red$aprjun21 <- 0
red$aprjun21[red$year_mon >= "2021-04" & red$year_mon <= "2021-06"] <- 1

# #####
# #NEW
red$julsep21 <- 0
red$julsep21[red$year_mon >= "2021-07" & red$year_mon <= "2021-09"] <- 1
# 
red$octdec21 <- 0
red$octdec21[red$year_mon >= "2021-10" & red$year_mon <= "2021-12"] <- 1
# 
# #####

# dummy for each month in treated period
red$mar2020 <- 0
red$mar2020[red$year_mon == "2020-03"] <- 1

red$apr2020 <- 0
red$apr2020[red$year_mon == "2020-04"] <- 1

red$may2020 <- 0
red$may2020[red$year_mon == "2020-05"] <- 1

red$jun2020 <- 0
red$jun2020[red$year_mon == "2020-06"] <- 1

red$jul2020 <- 0
red$jul2020[red$year_mon == "2020-07"] <- 1

red$aug2020 <- 0
red$aug2020[red$year_mon == "2020-08"] <- 1

red$sep2020 <- 0
red$sep2020[red$year_mon == "2020-09"] <- 1

red$oct2020 <- 0
red$oct2020[red$year_mon == "2020-10"] <- 1

red$nov2020 <- 0
red$nov2020[red$year_mon == "2020-11"] <- 1

red$dec2020 <- 0
red$dec2020[red$year_mon == "2020-12"] <- 1

red$jan2021 <- 0
red$jan2021[red$year_mon == "2021-01"] <- 1

red$feb2021 <- 0
red$feb2021[red$year_mon == "2021-02"] <- 1

red$mar2021 <- 0
red$mar2021[red$year_mon == "2021-03"] <- 1

red$apr2021 <- 0
red$apr2021[red$year_mon == "2021-04"] <- 1

red$may2021 <- 0
red$may2021[red$year_mon == "2021-05"] <- 1

red$jun2021 <- 0
red$jun2021[red$year_mon == "2021-06"] <- 1

# #####
# # NEW
red$jul2021 <- 0
red$jul2021[red$year_mon == "2021-07"] <- 1

red$aug2021 <- 0
red$aug2021[red$year_mon == "2021-08"] <- 1

red$sep2021 <- 0
red$sep2021[red$year_mon == "2021-09"] <- 1

red$oct2021 <- 0
red$oct2021[red$year_mon == "2021-10"] <- 1

red$nov2021 <- 0
red$nov2021[red$year_mon == "2021-11"] <- 1

red$dec2021 <- 0
red$dec2021[red$year_mon == "2021-12"] <- 1

# district prep -----------------------------------------------------------

# add zeors to KRS AGS
red$AGS_krs <- as.character(red$kid2019)
red$AGS_krs <- ifelse(test = nchar(red$AGS_krs) == 4,
                      yes = paste0("0", red$AGS_krs),
                      no = paste0(red$AGS_krs))

# make sf -----------------------------------------------------------------
red.sf <- st_set_geometry(red, red$geometry)

################################################################
# Düsseldorf                                                   #
################################################################

# subset for Düsseldorf 
red_dus <- subset(red.sf, red.sf$closest_main_airports == "EDDL")

# subset contour for Düsseldorf
cont_dus <- subset(airport_contour, airport_contour$ICAO == "EDDL")

# apply function
red_dus <- intersection_rings(housing_data = red_dus, contour_data = cont_dus)

# export data -------------------------------------------------------------
# saveRDS(red_dus, file = file.path(dataFlug, "housing/Temp/wk_dus.rds"))


################################################################
# München                                                      #
################################################################


red_muc <- subset(red.sf, red.sf$closest_main_airports == "EDDM")

# subset contour for München
cont_muc <- subset(airport_contour, airport_contour$ICAO == "EDDM")

# apply function
red_muc <- intersection_rings(housing_data = red_muc, contour_data = cont_muc)

# export data -------------------------------------------------------------
# saveRDS(red_muc, file = file.path(dataFlug, "housing/Temp/wk_muc.rds"))


################################################################
# Frankfurt                                                    #
################################################################

red_fra <- subset(red.sf, red.sf$closest_main_airports == "EDDF")

# subset contour for Frankfurt
cont_fra <- subset(airport_contour, airport_contour$ICAO == "EDDF")

# apply function
red_fra <- intersection_rings(housing_data = red_fra, contour_data = cont_fra)


# export data -------------------------------------------------------------
# saveRDS(red_fra, file = file.path(dataFlug, "housing/Temp/wk_fra.rds"))

################################################################
# Hamburg                                                      #
################################################################

red_ham <- subset(red.sf, red.sf$closest_main_airports == "EDDH")

# subset contour for Hamburg
cont_ham <- subset(airport_contour, airport_contour$ICAO == "EDDH")

# apply function
red_ham <- intersection_rings(housing_data = red_ham, contour_data = cont_ham)


# export data -------------------------------------------------------------
# saveRDS(red_ham, file = file.path(dataFlug, "housing/Temp/wk_ham.rds"))

################################################################
# Hannover                                                     #
################################################################

red_haj <- subset(red.sf, red.sf$closest_main_airports == "EDDV")

# subset contour for Hannover
cont_haj <- subset(airport_contour, airport_contour$ICAO == "EDDV")

# identify the rings
cont_haj$ring_nr <- NA
cont_haj$ring_nr[c(4,5,10)] <- 5 # outermost ring
cont_haj$ring_nr[9] <- 4
cont_haj$ring_nr[c(3,8)] <- 3
cont_haj$ring_nr[c(2,7)] <- 2
cont_haj$ring_nr[c(1,6)] <- 1 # innermost ring


# intersect the contour rings with the housing
# ring 1 (innermost)
int_1 <- st_intersects(red_haj, cont_haj[c(1,6),])
red_haj$con_ring1 <- lengths(int_1)
red_haj$con_ring1[red_haj$con_ring1 != 0] <- 1

# ring 2
int_2 <- st_intersects(red_haj, cont_haj[c(2,7),])
red_haj$con_ring2 <- lengths(int_2)
red_haj$con_ring2[red_haj$con_ring2 != 0] <- 1
red_haj$con_ring2[red_haj$con_ring1 == 1] <- 0

# ring 3
int_3 <- st_intersects(red_haj, cont_haj[c(3,8),])
red_haj$con_ring3 <- lengths(int_3)
red_haj$con_ring3[red_haj$con_ring3 != 0] <- 1
red_haj$con_ring3[red_haj$con_ring1 == 1 | red_haj$con_ring2 == 1] <- 0

# ring 4
int_4 <- st_intersects(red_haj, cont_haj[9,])
red_haj$con_ring4 <- lengths(int_4)
red_haj$con_ring4[red_haj$con_ring4 != 0] <- 1
red_haj$con_ring4[red_haj$con_ring1 == 1 | red_haj$con_ring2 == 1 | red_haj$con_ring3 == 1] <- 0

# ring 5 (outermost)
int_5 <- st_intersects(red_haj, cont_haj[c(4,5,10),])
red_haj$con_ring5 <- lengths(int_5)
red_haj$con_ring5[red_haj$con_ring5 != 0 ] <- 1
red_haj$con_ring5[red_haj$con_ring1 == 1 | red_haj$con_ring2 == 1 | red_haj$con_ring3 == 1 | red_haj$con_ring4 == 1] <- 0

# ring 6 (combination of the innermost and the second one)
red_haj$con_ring6 <- 0
red_haj$con_ring6[red_haj$con_ring1 == 1 | red_haj$con_ring2 == 1] <- 1

# ring 7 (combination of rings 1 to 3 (i.e. the inner ones))
red_haj$con_ring7 <- 0
red_haj$con_ring7[red_haj$con_ring1 == 1 | red_haj$con_ring2 == 1 | red_haj$con_ring3 == 1] <- 1

# ring 0 (represents the entire group of treated i.e. independent of in which exact ring they are)
red_haj$con_ring0 <- 0
red_haj$con_ring0[red_haj$con_ring1 == 1 | 
                    red_haj$con_ring2 == 1 | 
                    red_haj$con_ring3 == 1 | 
                    red_haj$con_ring4 == 1 | 
                    red_haj$con_ring5 == 1] <- 1


# export data -------------------------------------------------------------
# saveRDS(red_haj, file = file.path(dataFlug, "housing/Temp/wk_haj.rds"))


################################################################
# Leipzig                                                      #
################################################################

red_lej <- subset(red.sf, red.sf$closest_main_airports == "EDDP")

# subset contour for Leipzig
cont_lej <- subset(airport_contour, airport_contour$ICAO == "EDDP")

# apply function
red_lej <- intersection_rings(housing_data = red_lej, contour_data = cont_lej)


# export data -------------------------------------------------------------
# saveRDS(red_lej, file = file.path(dataFlug, "housing/Temp/wk_lej.rds"))


################################################################
# Koeln/ Bonn                                                  #
################################################################

red_cgn <- subset(red.sf, red.sf$closest_main_airports == "EDDK")

# subset contour for Leipzig
cont_cgn <- subset(airport_contour, airport_contour$ICAO == "EDDK")

# apply function
red_cgn <- intersection_rings(housing_data = red_cgn, contour_data = cont_cgn)


# export data -------------------------------------------------------------
# saveRDS(red_cgn, file = file.path(dataFlug, "housing/Temp/wk_cgn.rds"))

################################################################
# Stuttgart                                                    #
################################################################

red_str <- subset(red.sf, red.sf$closest_main_airports == "EDDS")

# subset contour for Leipzig
cont_str <- subset(airport_contour, airport_contour$ICAO == "EDDS")


# identify the rings
cont_str$ring_nr <- NA
cont_str$ring_nr[1] <- 5 # outermost ring
cont_str$ring_nr[2] <- 4
cont_str$ring_nr[3] <- 3
cont_str$ring_nr[4] <- 2
cont_str$ring_nr[c(5,6,7)] <- 1 # innermost ring


# intersect the contour rings with the housing
# ring 1 (innermost)
int_1 <- st_intersects(red_str, cont_str[c(5,6,7),])
red_str$con_ring1 <- lengths(int_1)
red_str$con_ring1[red_str$con_ring1 != 0] <- 1

# ring 2
int_2 <- st_intersects(red_str, cont_str[4,])
red_str$con_ring2 <- lengths(int_2)
red_str$con_ring2[red_str$con_ring2 != 0] <- 1
red_str$con_ring2[red_str$con_ring1 == 1] <- 0

# ring 3
int_3 <- st_intersects(red_str, cont_str[3,])
red_str$con_ring3 <- lengths(int_3)
red_str$con_ring3[red_str$con_ring3 != 0] <- 1
red_str$con_ring3[red_str$con_ring1 == 1 | red_str$con_ring2 == 1] <- 0

# ring 4
int_4 <- st_intersects(red_str, cont_str[2,])
red_str$con_ring4 <- lengths(int_4)
red_str$con_ring4[red_str$con_ring4 != 0] <- 1
red_str$con_ring4[red_str$con_ring1 == 1 | red_str$con_ring2 == 1 | red_str$con_ring3 == 1] <- 0

# ring 5 (outermost)
int_5 <- st_intersects(red_str, cont_str[1,])
red_str$con_ring5 <- lengths(int_5)
red_str$con_ring5[red_str$con_ring5 != 0 ] <- 1
red_str$con_ring5[red_str$con_ring1 == 1 | red_str$con_ring2 == 1 | red_str$con_ring3 == 1 | red_str$con_ring4 == 1] <- 0

# ring 6 (combination of the innermost and the second one)
red_str$con_ring6 <- 0
red_str$con_ring6[red_str$con_ring1 == 1 | red_str$con_ring2 == 1] <- 1

# ring 7 (combination of rings 1 to 3 (i.e. the inner ones))
red_str$con_ring7 <- 0
red_str$con_ring7[red_str$con_ring1 == 1 | red_str$con_ring2 == 1 | red_str$con_ring3 == 1] <- 1

# ring 0 (represents the entire group of treated i.e. independent of in which exact ring they are)
red_str$con_ring0 <- 0
red_str$con_ring0[red_str$con_ring1 == 1 | 
                    red_str$con_ring2 == 1 | 
                    red_str$con_ring3 == 1 | 
                    red_str$con_ring4 == 1 | 
                    red_str$con_ring5 == 1] <- 1


# export data -------------------------------------------------------------
# saveRDS(red_str, file = file.path(dataFlug, "housing/Temp/wk_str.rds"))


################################################################
# Nürnberg                                                     #
################################################################

red_nue <- subset(red.sf, red.sf$closest_main_airports == "EDDN")

# subset contour for Berlin
cont_nue <- subset(airport_contour, airport_contour$ICAO == "EDDN")


# apply function
red_nue <- intersection_rings(housing_data = red_nue, contour_data = cont_nue)


# export data -------------------------------------------------------------
# saveRDS(red_nue, file = file.path(dataFlug, "housing/Temp/wk_nue.rds"))


################################################################
# Berlin-Schönefeld and Berlin-Tegel                           #
################################################################

# SXF ---------------------------------------------------------------------
red_sxf <- subset(red.sf, red.sf$closest_main_airports == "EDDB")

# subset contour for Berlin
cont_sxf <- subset(airport_contour, airport_contour$ICAO == "EDDB")

# identify the rings
cont_sxf$ring_nr <- NA
cont_sxf$ring_nr[2] <- 5 # outermost ring
cont_sxf$ring_nr[3] <- 4
cont_sxf$ring_nr[c(1,4,6)] <- 3
cont_sxf$ring_nr[c(5,7)] <- 2
cont_sxf$ring_nr[c(8,9)] <- 1 # innermost ring


# intersect the contour rings with the housing
# ring 1 (innermost)
int_1 <- st_intersects(red_sxf, cont_sxf[c(8,9),])
red_sxf$con_ring1 <- lengths(int_1)
red_sxf$con_ring1[red_sxf$con_ring1 != 0] <- 1

# ring 2
int_2 <- st_intersects(red_sxf, cont_sxf[c(5,7),])
red_sxf$con_ring2 <- lengths(int_2)
red_sxf$con_ring2[red_sxf$con_ring2 != 0] <- 1
red_sxf$con_ring2[red_sxf$con_ring1 == 1] <- 0

# ring 3
int_3 <- st_intersects(red_sxf, cont_sxf[c(1,4,6),])
red_sxf$con_ring3 <- lengths(int_3)
red_sxf$con_ring3[red_sxf$con_ring3 != 0] <- 1
red_sxf$con_ring3[red_sxf$con_ring1 == 1 | red_sxf$con_ring2 == 1] <- 0

# ring 4
int_4 <- st_intersects(red_sxf, cont_sxf[3,])
red_sxf$con_ring4 <- lengths(int_4)
red_sxf$con_ring4[red_sxf$con_ring4 != 0] <- 1
red_sxf$con_ring4[red_sxf$con_ring1 == 1 | red_sxf$con_ring2 == 1 | red_sxf$con_ring3 == 1] <- 0

# ring 5 (outermost)
int_5 <- st_intersects(red_sxf, cont_sxf[2,])
red_sxf$con_ring5 <- lengths(int_5)
red_sxf$con_ring5[red_sxf$con_ring5 != 0 ] <- 1
red_sxf$con_ring5[red_sxf$con_ring1 == 1 | red_sxf$con_ring2 == 1 | red_sxf$con_ring3 == 1 | red_sxf$con_ring4 == 1] <- 0

# ring 6 (combination of the innermost and the second one)
red_sxf$con_ring6 <- 0
red_sxf$con_ring6[red_sxf$con_ring1 == 1 | red_sxf$con_ring2 == 1] <- 1

# ring 7 (combination of rings 1 to 3 (i.e. the inner ones))
red_sxf$con_ring7 <- 0
red_sxf$con_ring7[red_sxf$con_ring1 == 1 | red_sxf$con_ring2 == 1 | red_sxf$con_ring3 == 1] <- 1

# ring 0 (represents the entire group of treated i.e. independent of in which exact ring they are)
red_sxf$con_ring0 <- 0
red_sxf$con_ring0[red_sxf$con_ring1 == 1 | 
                    red_sxf$con_ring2 == 1 | 
                    red_sxf$con_ring3 == 1 | 
                    red_sxf$con_ring4 == 1 | 
                    red_sxf$con_ring5 == 1] <- 1


# Tegel -------------------------------------------------------------------

red_txl <- subset(red.sf, red.sf$closest_main_airports == "EDDT")

# subset contour for Berlin
cont_txl <- subset(airport_contour, airport_contour$ICAO == "EDDT")

# apply function
red_txl <- intersection_rings(housing_data = red_txl, contour_data = cont_txl)


################################################################
# combine                                                      #
################################################################

# combine
red_contour <- rbind(red_dus, red_muc, red_fra, red_ham, red_haj, red_lej, red_cgn, red_str, red_nue, red_sxf, red_txl)


################################################################
# export                                                       #
################################################################

saveRDS(red_contour, file.path(dataFlug, "housing/wk_contour.rds"))


################################################################
# clear everything                                             #
################################################################

rm(list=ls())


