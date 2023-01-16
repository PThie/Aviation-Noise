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