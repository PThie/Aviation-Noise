prep_est <- function(housing_data){
    #' @title Preparation for estimation
    #' 
    #' @description This function prepares the data for estimation.
    #' 
    #' @param housing_data Housing data after contour information and additonal
    #' variables have been added
    #'   
    #' @return Returns housing data ready for estimation
    #' 
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # drop geometry
    housing_data <- st_drop_geometry(housing_data)

    # make "months" factor variable
    housing_data$months <- as.factor(housing_data$year_mon_end)
    
    # restrict sample
    housing_data <- housing_data |>
        # restrict to 5km to contour ring
        filter(distance_main_airports <= 5) |>
        # exlcude 1km buffer
        filter(distance_main_airports >= 1 | distance_main_airports == 0) |>
        # drop March 2020
        filter(year_mon_end != "2020-03") |>
        # drop Airports Tegel and Schoenefeld
        filter(closest_main_airports != "EDDT" & closest_main_airports != "EDDB")
    
    # add ring for 60dB and above
    housing_data <- housing_data |>
        mutate(
            con_ring8 = case_when(
                con_ring1 == 1 |
                con_ring2 == 1 |
                con_ring3 == 1 |
                con_ring4 == 1 ~ 1,
                con_ring5 == 1 ~ 0
            )
        )

    housing_data$con_ring8[is.na(housing_data$con_ring8)] <- 0

    # return
    return(housing_data)
}