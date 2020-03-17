#' Make Reservoir Flow Duration  curve
#'
#'Plots up a duration curve for the columns of y_data that will be used for reservoir parametrization

#' @inheritParams D4TraverseB
#' @param  outlets x,y coordinates of the outlet points or points to mask upstream areas for if there is just one point this can be input as c(x,y), if there are multiple points, this should be a matrix with a separate row for each point
#' @param printflag Optional flag to print out the number of cells in the queue durring iterations, defaults to F
#' @export
#'

Plot.FlowDur=function(Release, ){




