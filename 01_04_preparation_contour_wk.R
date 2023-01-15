################################################################
# Load Data                                                    #
################################################################

# load data
red <- qs::qread(
    file.path(
        data_path, "housing/WK_prepared.qs"
    )
)

# geo info
krs <- st_read(
    file.path(
        data_gebiete, "Kreis/2019/VG250_KRS.shp"
    ),
    quiet = TRUE
)
gem <- st_read(
    file.path(
        data_gebiete, "Gemeinde/2019/VG250_GEM.shp"
    ),
    quiet = TRUE
)

# contour data
airport_contour <- qs::qread(
    file.path(
        data_path, "Contour_Maps/all_airports_contour.qs"
    )
)

################################################################
# Preparation                                                  #
################################################################

# move to different file
# # construct time variables ---------------------------------------------
# red$fir_lockdown <- 0
# red$fir_lockdown[red$year_mon >= "2020-03"] <- 1

# # January to March 2018
# red$janmar18 <- 0
# red$janmar18[red$year_mon >= "2018-01" & red$year_mon <= "2018-03"] <- 1

# # April to June 2018
# red$aprjun18 <- 0
# red$aprjun18[red$year_mon >= "2018-04" & red$year_mon <= "2018-06"] <- 1

# # July to September 2018
# red$julsep18 <- 0
# red$julsep18[red$year_mon >= "2018-07" & red$year_mon <= "2018-09"] <- 1

# # October to December 2018
# red$octdec18 <- 0
# red$octdec18[red$year_mon >= "2018-10" & red$year_mon <= "2018-12"] <- 1

# # January to March 2019
# red$janmar19 <- 0
# red$janmar19[red$year_mon >= "2019-01" & red$year_mon <= "2019-03"] <- 1

# # April to June 2019
# red$aprjun19 <- 0
# red$aprjun19[red$year_mon >= "2019-04" & red$year_mon <= "2019-06"] <- 1

# # July to September 2019
# red$julsep19 <- 0
# red$julsep19[red$year_mon >= "2019-07" & red$year_mon <= "2019-09"] <- 1

# # October to December 2019
# red$octdec19 <- 0
# red$octdec19[red$year_mon >= "2019-10" & red$year_mon <= "2019-12"] <- 1

# # January to February 2020
# red$janfeb <- 0
# red$janfeb[red$year_mon >= "2020-01" & red$year_mon <= "2020-02"] <- 1

# # March to December 2020
# red$mardec <- 0
# red$mardec[red$year_mon >= "2020-03"] <- 1

# # March to May 2020
# red$marmay <- 0
# red$marmay[red$year_mon >= "2020-03" & red$year_mon <= "2020-05"] <- 1

# # June to September 2020
# red$junsep <- 0
# red$junsep[red$year_mon >= "2020-06" & red$year_mon <= "2020-09"] <- 1

# # October to December 2020
# red$octdec <- 0
# red$octdec[red$year_mon >= "2020-10" & red$year_mon <= "2020-12"] <- 1

# # January to March 2021
# red$janmar21 <- 0
# red$janmar21[red$year_mon >= "2021-01" & red$year_mon <= "2021-03"] <- 1

# # April to June 2021
# red$aprjun21 <- 0
# red$aprjun21[red$year_mon >= "2021-04" & red$year_mon <= "2021-06"] <- 1

# # #####
# # #NEW
# red$julsep21 <- 0
# red$julsep21[red$year_mon >= "2021-07" & red$year_mon <= "2021-09"] <- 1
# # 
# red$octdec21 <- 0
# red$octdec21[red$year_mon >= "2021-10" & red$year_mon <= "2021-12"] <- 1






# # district prep -----------------------------------------------------------

# # add zeors to KRS AGS
# red$AGS_krs <- as.character(red$kid2019)
# red$AGS_krs <- ifelse(test = nchar(red$AGS_krs) == 4,
#                       yes = paste0("0", red$AGS_krs),
#                       no = paste0(red$AGS_krs))

################################################################
# Functions                                                    #
################################################################

# filtering housing data
housing_cont <- function(icao_id) {
    #' @description This function combines the housing data and the airport
    #' contour data such that each housing unit is assigned to the contour of
    #' the nearest airport
    #' 
    #' @param icao Airport identifier (ICAO ID)
    #' 
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # subset data

    # housing data
    hous <- red |>
        filter(closest_main_airports == icao_id)

    # contour data
    cont <- airport_contour |>
        filter(icao == icao_id)

    #----------------------------------------------
    # intersect both data sets
    int_data <- intersection_rings(
        housing_data = hous, contour_data = cont
    )

    #----------------------------------------------
    # return
    return(int_data)
}

################################################################
# Airport intersections                                        #
################################################################

#----------------------------------------------
# Duesseldorf
red_dus <- housing_cont(icao_id = "EDDL")

#----------------------------------------------
# Munich
red_muc <- housing_cont(icao_id = "EDDM")

#----------------------------------------------
# Frankfurt
red_fra <- housing_cont(icao_id = "EDDF")

#----------------------------------------------
# Hamburg
red_ham <- housing_cont(icao_id = "EDDH")

#----------------------------------------------
# Hannover (special case)

# subset data
red_haj <- red |>
    filter(closest_main_airports == "EDDV")
cont_haj <- airport_contour |>
    filter(icao == "EDDV")

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
red_haj$con_ring3[
    red_haj$con_ring1 == 1 |
    red_haj$con_ring2 == 1
] <- 0

# ring 4
int_4 <- st_intersects(red_haj, cont_haj[9,])
red_haj$con_ring4 <- lengths(int_4)
red_haj$con_ring4[red_haj$con_ring4 != 0] <- 1
red_haj$con_ring4[
    red_haj$con_ring1 == 1 |
    red_haj$con_ring2 == 1 |
    red_haj$con_ring3 == 1
] <- 0

# ring 5 (outermost)
int_5 <- st_intersects(red_haj, cont_haj[c(4,5,10),])
red_haj$con_ring5 <- lengths(int_5)
red_haj$con_ring5[red_haj$con_ring5 != 0 ] <- 1
red_haj$con_ring5[
    red_haj$con_ring1 == 1 |
    red_haj$con_ring2 == 1 |
    red_haj$con_ring3 == 1 |
    red_haj$con_ring4 == 1
] <- 0

# ring 6 (combination of the innermost and the second one)
red_haj$con_ring6 <- 0
red_haj$con_ring6[
    red_haj$con_ring1 == 1 |
    red_haj$con_ring2 == 1
] <- 1

# ring 7 (combination of rings 1 to 3 (i.e. the inner ones))
red_haj$con_ring7 <- 0
red_haj$con_ring7[
    red_haj$con_ring1 == 1 |
    red_haj$con_ring2 == 1 |
    red_haj$con_ring3 == 1
] <- 1

# ring 0 (represents the entire group of treated i.e. independent of in which exact ring they are)
red_haj$con_ring0 <- 0
red_haj$con_ring0[
    red_haj$con_ring1 == 1 | 
    red_haj$con_ring2 == 1 | 
    red_haj$con_ring3 == 1 | 
    red_haj$con_ring4 == 1 | 
    red_haj$con_ring5 == 1
] <- 1

#----------------------------------------------
# Leipzig
red_lej <- housing_cont(icao_id = "EDDP")

#----------------------------------------------
# Cologne
red_cgn <- housing_cont(icao_id = "EDDK")

#----------------------------------------------
# Stuttgar (special case)

# subset data
red_str <- red |>
    filter(closest_main_airports == "EDDS")
cont_str <- airport_contour |>
    filter(icao == "EDDS")

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
red_str$con_ring3[
    red_str$con_ring1 == 1 |
    red_str$con_ring2 == 1
] <- 0

# ring 4
int_4 <- st_intersects(red_str, cont_str[2,])
red_str$con_ring4 <- lengths(int_4)
red_str$con_ring4[red_str$con_ring4 != 0] <- 1
red_str$con_ring4[
    red_str$con_ring1 == 1 |
    red_str$con_ring2 == 1 |
    red_str$con_ring3 == 1
] <- 0

# ring 5 (outermost)
int_5 <- st_intersects(red_str, cont_str[1,])
red_str$con_ring5 <- lengths(int_5)
red_str$con_ring5[red_str$con_ring5 != 0 ] <- 1
red_str$con_ring5[
    red_str$con_ring1 == 1 |
    red_str$con_ring2 == 1 |
    red_str$con_ring3 == 1 |
    red_str$con_ring4 == 1
] <- 0

# ring 6 (combination of the innermost and the second one)
red_str$con_ring6 <- 0
red_str$con_ring6[
    red_str$con_ring1 == 1 |
    red_str$con_ring2 == 1
] <- 1

# ring 7 (combination of rings 1 to 3 (i.e. the inner ones))
red_str$con_ring7 <- 0
red_str$con_ring7[
    red_str$con_ring1 == 1 |
    red_str$con_ring2 == 1 |
    red_str$con_ring3 == 1
] <- 1

# ring 0 (represents the entire group of treated i.e. independent of in which exact ring they are)
red_str$con_ring0 <- 0
red_str$con_ring0[
    red_str$con_ring1 == 1 | 
    red_str$con_ring2 == 1 | 
    red_str$con_ring3 == 1 | 
    red_str$con_ring4 == 1 | 
    red_str$con_ring5 == 1
] <- 1

#----------------------------------------------
# Nuernberg
red_nue <- housing_cont(icao_id = "EDDN")

#----------------------------------------------
# Berlin-Schoenefeld (special case)

# subset data
red_sxf <- red |>
    filter(closest_main_airports == "EDDB")
cont_sxf <- airport_contour |>
    filter(icao == "EDDB")

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
red_sxf$con_ring3[
    red_sxf$con_ring1 == 1 |
    red_sxf$con_ring2 == 1
] <- 0

# ring 4
int_4 <- st_intersects(red_sxf, cont_sxf[3,])
red_sxf$con_ring4 <- lengths(int_4)
red_sxf$con_ring4[red_sxf$con_ring4 != 0] <- 1
red_sxf$con_ring4[
    red_sxf$con_ring1 == 1 |
    red_sxf$con_ring2 == 1 |
    red_sxf$con_ring3 == 1
] <- 0

# ring 5 (outermost)
int_5 <- st_intersects(red_sxf, cont_sxf[2,])
red_sxf$con_ring5 <- lengths(int_5)
red_sxf$con_ring5[red_sxf$con_ring5 != 0 ] <- 1
red_sxf$con_ring5[
    red_sxf$con_ring1 == 1 |
    red_sxf$con_ring2 == 1 |
    red_sxf$con_ring3 == 1 |
    red_sxf$con_ring4 == 1
] <- 0

# ring 6 (combination of the innermost and the second one)
red_sxf$con_ring6 <- 0
red_sxf$con_ring6[
    red_sxf$con_ring1 == 1 |
    red_sxf$con_ring2 == 1
] <- 1

# ring 7 (combination of rings 1 to 3 (i.e. the inner ones))
red_sxf$con_ring7 <- 0
red_sxf$con_ring7[
    red_sxf$con_ring1 == 1 |
    red_sxf$con_ring2 == 1 |
    red_sxf$con_ring3 == 1
] <- 1

# ring 0 (represents the entire group of treated i.e. independent of in which exact ring they are)
red_sxf$con_ring0 <- 0
red_sxf$con_ring0[
    red_sxf$con_ring1 == 1 | 
    red_sxf$con_ring2 == 1 | 
    red_sxf$con_ring3 == 1 | 
    red_sxf$con_ring4 == 1 | 
    red_sxf$con_ring5 == 1
] <- 1

#----------------------------------------------
# Berlin-Tegel
red_txl <- housing_cont(icao_id = "EDDT")

################################################################
# combine                                                      #
################################################################

# combine
red_contour <- rbind(
    red_dus, red_muc, red_fra, red_ham, red_haj, red_lej,
    red_cgn, red_str, red_nue, red_sxf, red_txl
)

################################################################
# export                                                       #
################################################################

qs::qsave(
    red_contour,
    file.path(
        data_path,
        "housing/WK_contour.qs"
    )
)