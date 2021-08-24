## ---------------------------
##
## COVID19 Event Risk Assessment Planning tool
##
## Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
## Lavanya Rishsishwar <lrishishwar@ihrc.com>
## Model from Joshua Weitz
## See: https://github.com/jsweitz/covid-19-ga-summer-2020
## ---------------------------
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(sever)
library(shinypanels)
options(scipen = 999)

event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000)


source('ui/navbar-custom-css.R', local = T)
source('ui/usa-map-tab.R', local = T)
source('ui/risk-game-tab.R', local = T)
source('ui/global-map-tab.R', local = T)
source('ui/usa-real-time-tab.R', local = T)
source('ui/usa-continuous-tab.R', local = T)
source('ui/tutorial-tab.R', local = T)
source('ui/about-tabset.R', local = T)

### R functions
# add in methods from https://github.com/rstudio/leaflet/pull/598
setCircleMarkerRadius <-
  function(map, layerId, radius, data = getMapData(map)) {
    options <- list(layerId = layerId, radius = radius)
    # evaluate all options
    options <- evalFormula(options, data = data)
    # make them the same length (by building a data.frame)
    options <-
      do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))
    leaflet::invokeMethod(map, data, "setRadius", options$layerId, options$radius)
  }

setCircleMarkerStyle <- function(map,
                                 layerId
                                 ,
                                 radius = NULL
                                 ,
                                 stroke = NULL
                                 ,
                                 color = NULL
                                 ,
                                 weight = NULL
                                 ,
                                 opacity = NULL
                                 ,
                                 fill = NULL
                                 ,
                                 fillColor = NULL
                                 ,
                                 fillOpacity = NULL
                                 ,
                                 dashArray = NULL
                                 ,
                                 options = NULL
                                 ,
                                 data = getMapData(map)) {
  if (!is.null(radius)) {
    setCircleMarkerRadius(map,
                          layerId = layerId,
                          radius = radius,
                          data = data)
  }

  options <- c(list(layerId = layerId),
               options,
               filterNULL(
                 list(
                   stroke = stroke,
                   color = color,
                   weight = weight,
                   opacity = opacity,
                   fill = fill,
                   fillColor = fillColor,
                   fillOpacity = fillOpacity,
                   dashArray = dashArray
                 )
               ))

  if (length(options) < 2) {
    # no style options set
    return()
  }
  # evaluate all options
  options <- evalFormula(options, data = data)

  # make them the same length (by building a data.frame)
  options <-
    do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))
  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "marker", layerId, style)

}

setShapeStyle <- function(map,
                          data = getMapData(map),
                          layerId,
                          stroke = NULL,
                          color = NULL,
                          weight = NULL,
                          opacity = NULL,
                          fill = NULL,
                          fillColor = NULL,
                          fillOpacity = NULL,
                          dashArray = NULL,
                          smoothFactor = NULL,
                          noClip = NULL,
                          options = NULL) {
  options <- c(list(layerId = layerId),
               options,
               filterNULL(
                 list(
                   stroke = stroke,
                   color = color,
                   weight = weight,
                   opacity = opacity,
                   fill = fill,
                   fillColor = fillColor,
                   fillOpacity = fillOpacity,
                   dashArray = dashArray,
                   smoothFactor = smoothFactor,
                   noClip = noClip
                 )
               ))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <-
    do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))

  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style)

}

### JS methods
leafletjs <-  tags$head(# add in methods from https://github.com/rstudio/leaflet/pull/598
  tags$script(
    HTML(
      '
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);
  //console.log(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.setStyle(style[i]);
    }
  });
};

window.LeafletWidget.methods.setRadius = function(layerId, radius){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
    radius = [radius];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer("marker", d);
    if (layer){ // or should this raise an error?
      layer.setRadius(radius[i]);
    }
  });
};

window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.unbindTooltip();
      // the object subsetting to get the integer array and casting to string is what I added
      layer.bindTooltip(label.label[i].toString());
    }
  });
};
'
    )
  ))





shinyUI(
  navbarPage(
    theme = shinytheme("sandstone"),
    selected = "usa",
    collapsible = TRUE,
    id = "nav-page",
    windowTitle = "COVID-19 Event Risk Assessment Planning Tool",
    title = "COVID-19 Event Risk Assessment Planning Tool",
    # leafletjs,
    header = div(
      tags$script("http://platform.twitter.com/widgets.js"),
      tags$head(
        includeHTML(("www/ga.html")),
        # when the image panel is toggled, trigger an invalidate() on the leaflet map
        tags$script(
          '$(".panel-header-dismiss").on("click", function() { $(this).trigger("shown"); });'
        ),
        use_sever(),
        tags$style(
          "
        .intro-text {
    font-size: large;
      }
  .loc-text {
  font-style: italic;
    font-weight: 200;
  }

  .loc-text.success {
      color: #28b78d;
  }
      .footer {
        left: 30px;
      }
      .app-container {
        overflow: hidden;
      }
      .panel-header-title {
        font-size: 2rem;
      }
      .fake-sidebar {
      font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif;
    font-size: 14px;
    line-height: 1.42857143;
    color: #333;
      }
      .context-header {
      font-size: 2rem;
      }
      "
        ),
        tags$style(HTML(navbar_css)),
        HTML(
          '<meta property="og:title" content="COVID-19 Event Risk Assessment Planning Tool" />
<meta property="og:type" content="website" />
<meta property="og:url" content="https://covid19risk.biosci.gatech.edu/" />
<meta property="og:image" content="https://covid19risk.biosci.gatech.edu/og.png" />'
        )
      )
    ),
    footer =      HTML(
      '<div class="footer well col-xs-12 col-sm-12 col-md-3"><p>The COVID-19 Event Risk Assessment Planning Tool is a collaborative project led by <a href="https://ecotheory.biosci.gatech.edu/" rel="noopener" target="_blank">Prof. Joshua Weitz</a> and <a href="http://friendlycities.gatech.edu/" rel="noopener" target="_blank">Prof. Clio Andris</a> at the Georgia Institute of Technology, along with researchers at the <a href="https://www.abil.ihrc.com/" rel="noopener" target="_blank">Applied Bioinformatics Laboratory</a>, Duke University, and Stanford University, and powered by <a href="https://rstudio.com/" rel="noopener" target="_blank">RStudio</a>.  Description of the method and analyses available at <a href="https://www.nature.com/articles/s41562-020-01000-9/" rel="noopener" target="_blank">Nature Human Behaviour</a>.</p>
          <p>Ongoing support for the project is via the CDC, Charities in Aid Foundation, and The Marier Cunningham Foundation.</p></div>'
    ),
    usa_map_tab,
    # risk_game_tab,
    global_map_tab,
    usa_real_time_tab,
    usa_continuous_tab,
    tutorial_tab,
    about_tabset

  )
)
