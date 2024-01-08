###############################################################
# Description                                                 #
###############################################################

# This file intersects the cleaned housing data with airport noise contour
# shapes to determine which housing unit lies within the shape (treated)
# and which does not (control).

################################################################
# Load Data                                                    #
################################################################

# load data
red <- qs::qread(
    file.path(
        data_path,
        "housing/HK_prepared.qs"
    )
)

# contour data
airport_contour <- qs::qread(
    file.path(
        data_path,
        "Contour_Maps/all_airports_contour.qs"
    )
)

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
        "housing/HK_contour.qs"
    )
)