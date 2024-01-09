read_housing <- function(filename) {
    #' @title Reading housing data
    #' 
    #' @description This function reads the prepared housing data.
    #' 
    #' @param filename Name of the housing data to ready
    #'   
    #' @return Returns housing data
    #' 
    #' @author Patrick Thiel
    
    #----------------------------------------------
    fln <- paste0(filename, ".qs")
    dta <- qs::qread(
        file.path(
            data_path,
            "housing",
            fln
        )
    )
    return(dta)
}