library(roxygen2)


intersection_rings <- function(housing_data, contour_data){
  
  ##### documentation
  #' Intersection function
  #'
  #' \code{intersection_rings} returns indicators for housing objects within
  #' airport contours
  #'
  #' @param housing_data Insert here the housing objects in sf-format.
  #' @param contour_data Insert here the contours of the airports in sf-format.
  #'
  #' @return Returns the housing data with 7 indicators for the contour rings.
  #' 
  #' @export
  
  ##### function itself
  # identify the rings
  contour_data$ring_nr <- NA
  contour_data$ring_nr[1] <- 5 # outermost ring
  contour_data$ring_nr[2] <- 4
  contour_data$ring_nr[3] <- 3
  contour_data$ring_nr[4] <- 2
  contour_data$ring_nr[5] <- 1 # innermost ring
  
  # intersections ----------------------------------------
  # intersect the contour rings with the housing
  # ring 1 (innermost)
  int_1 <- st_intersects(housing_data, contour_data[5,])
  housing_data$con_ring1 <- lengths(int_1)
  housing_data$con_ring1[housing_data$con_ring1 != 0] <- 1
  
  # ring 2
  int_2 <- st_intersects(housing_data, contour_data[4,])
  housing_data$con_ring2 <- lengths(int_2)
  housing_data$con_ring2[housing_data$con_ring2 != 0] <- 1
  housing_data$con_ring2[housing_data$con_ring1 == 1] <- 0
  
  # ring 3
  int_3 <- st_intersects(housing_data, contour_data[3,])
  housing_data$con_ring3 <- lengths(int_3)
  housing_data$con_ring3[housing_data$con_ring3 != 0] <- 1
  housing_data$con_ring3[housing_data$con_ring1 == 1 |
                           housing_data$con_ring2 == 1] <- 0
  
  # ring 4
  int_4 <- st_intersects(housing_data, contour_data[2,])
  housing_data$con_ring4 <- lengths(int_4)
  housing_data$con_ring4[housing_data$con_ring4 != 0] <- 1
  housing_data$con_ring4[housing_data$con_ring1 == 1 | 
                           housing_data$con_ring2 == 1 |
                           housing_data$con_ring3 == 1] <- 0
  
  # ring 5 (outermost)
  int_5 <- st_intersects(housing_data, contour_data[1,])
  housing_data$con_ring5 <- lengths(int_5)
  housing_data$con_ring5[housing_data$con_ring5 != 0 ] <- 1
  housing_data$con_ring5[housing_data$con_ring1 == 1 | 
                           housing_data$con_ring2 == 1 | 
                           housing_data$con_ring3 == 1 | 
                           housing_data$con_ring4 == 1] <- 0
  
  # ring 6 (combination of the innermost and the second one)
  housing_data$con_ring6 <- 0
  housing_data$con_ring6[housing_data$con_ring1 == 1 | 
                           housing_data$con_ring2 == 1] <- 1
  
  # ring 7 (combination of rings 1 to 3 (i.e. the inner ones))
  housing_data$con_ring7 <- 0
  housing_data$con_ring7[housing_data$con_ring1 == 1 |
                           housing_data$con_ring2 == 1 | 
                           housing_data$con_ring3 == 1] <- 1
  
  # ring 0 (represents the entire group of treated i.e. independent of in which exact ring they are)
  housing_data$con_ring0 <- 0
  housing_data$con_ring0[housing_data$con_ring1 == 1 | 
                           housing_data$con_ring2 == 1 | 
                           housing_data$con_ring3 == 1 | 
                           housing_data$con_ring4 == 1 | 
                           housing_data$con_ring5 == 1] <- 1
  
  
  # return ------------------------------------------
  
  return(housing_data)  
  
}
