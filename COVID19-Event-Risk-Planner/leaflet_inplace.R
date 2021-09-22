### R functions
# add in methods from https://github.com/rstudio/leaflet/pull/598
setCircleMarkerRadius <- function(map, layerId, radius, data=getMapData(map)){
  options <- list(layerId = layerId, radius = radius)
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  leaflet::invokeMethod(map, data, "setRadius", options$layerId, options$radius)
}

setCircleMarkerStyle <- function(map, layerId
                                 , radius = NULL
                                 , stroke = NULL
                                 , color = NULL
                                 , weight = NULL
                                 , opacity = NULL
                                 , fill = NULL
                                 , fillColor = NULL
                                 , fillOpacity = NULL
                                 , dashArray = NULL
                                 , options = NULL
                                 , data = getMapData(map)
){
  if (!is.null(radius)){
    setCircleMarkerRadius(map, layerId = layerId, radius = radius, data = data)
  }

  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray
               )))

  if (length(options) < 2) { # no style options set
    return()
  }
  # evaluate all options
  options <- evalFormula(options, data = data)

  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "marker", layerId, style);
}

setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))

  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  print(list(style=style))

  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}


setShapeLabel <- function( map, data = getMapData(map), layerId,
                           label = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  # options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  # layerId <- options[[1]]
  # label <- options[-1] # drop layer column
  # message("invoke")
  # typo fixed in this line
  leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label);
}
