#' dygraphs Plot
#' 
#' ...
#' 
#' @param data data.frame
#' @param x optional character string identifying column in the data for x-axis (TODO: support for vector)
#' If not supplied, attempt is made to detect it from timeBased data columns or rownames
#' @param y optional character string identifying column in the data for y-axis series (TODO: support for vector)
#' @param y2 (not yet supported) optional character string identifying column in the data for secondary y-axis series (TODO: support for vector)
#' @param sync logical default FALSE. Set to TRUE and dygraph will react to highlights and redraws 
#' in other dygraphs on the same page. (TODO: supply vector of chartIds to sync this chart with)
#' @param ... further options passed to the dygraph options slot. See http://dygraphs.com/options.html
#' @params defaults logical. Should some dygraph options defaults be preloaded? Default is TRUE. 
#' Options supplied via ... will still override these defaults.
#' @export
dgPlot <- dyPlot <- dygraph <- dygraphPlot<- function(data, x, y, y2, sync=FALSE, defaults=TRUE, ...){
  
  myChart <- Dygraph$new()
  myChart$parseData(data, x, y, y2)
  if(defaults)
    myChart$setDefaults(...)
  myChart$setOpts(...) # dygraph javascript options
  # myChart$setLib( "." )
  # myChart$templates$script = "layouts/chart2.html"
  myChart$templates$script = system.file("/libraries/dygraph/layouts/chart2.html"
                                         , package = "rChartsDygraphs")
  myChart$setTemplate(afterScript = "<script></script>")
  if(sync)
    myChart$synchronize()
  
  return(myChart$copy())
}

Dygraph <- setRefClass('Dygraph', contains = 'rCharts'
                       , methods = list(
  initialize = function(){
    callSuper()
    params <<- c(params, list(options = list(width=params$width, height=params$height)))
  },
  getPayload = function(chartId){
    list(chartParams = toJSON2(params), chartId = chartId, lib = basename(lib), liburl = LIB$url)
  },
  parseData = function(data, x, y, y2){
    if(is.xts(data)) {
      t = index(data)
      data = cbind(t, as.data.frame(data))
    }
    if(missing(x)) #TODO: detect using xts:::timeBased
      x <- names(data)[1]
    if(missing(y))
      y = setdiff(names(data), x)
    data[[x]] <- paste0("#!new Date(", as.numeric(as.POSIXct(data[[x]])) * 1000, ")!#")
    data[y] = lapply(data[y], function(x) as.numeric(x)) # temp fix for logical values
    data <- data[,c(x, y)]
    params <<- modifyList(params, getLayer(x=x, data=data, y=y))
    setOpts(labels=c(x, y)) # because lodash drops column names
  },
  setDefaults = function(...){
    args = list(...)
    safe <- function(x) if (length(x)) x else FALSE
    
    # make floating legend more readable by default
    if(safe(args$legendFollow))
      setOpts(labelsDivStyles=list(
        pointerEvents='none', # let mouse events fall through the legend div
        # borderRadius='10px',
        # boxShadow='4px 4px 4px #888',
        # background='none',
        backgroundColor='rgba(255, 255, 255, 0.5)'
      ))
    if(!"rightGap" %in% names(args))
      setOpts(rightGap=20) # makes it easier to highlight the right-most data point.
  },
  setOpts = function(...){
    opts <- list(...)
    fix_dygraph_options <- function(x) {
      # dygraph colors parameter accepts JSON array only, no character string
      if(length(x$colors))
        x$colors <- as.list(x$colors)
      return(x)
    }
  
    params$options <<- modifyList(params$options, fix_dygraph_options(opts))
  },
  synchronize = function(){
    setOpts(
      highlightCallback = "#!
        function(e, x, pts, row) {
          for (var j = 0; j < gs.length; j++) {
            gs[j].setSelection(row);
          }
        }!#",
      unhighlightCallback = "#!
        function(e, x, pts, row) {
          for (var j = 0; j < gs.length; j++) {
            gs[j].clearSelection();
          };
        }!#",
      drawCallback = "#!
        function(me, initial) {
          if (blockRedraw || initial) return;
          blockRedraw = true;
          var range = me.xAxisRange();
          var yrange = me.yAxisRange();
          for (var j = 0; j < gs.length; j++) {
            if (gs[j] == me) continue;
            gs[j].updateOptions( {
              dateWindow: range
              // valueRange: yrange // we don't want to sync along y-axis
            } );
          }
          blockRedraw = false;
        }!#"
      )
  })
)

#' Display multiple dygraphs
#' 
#' ...
#' 
#' @param ... list of dygraph objects to display
layout_dygraphs <- function(...) {
  l = list(...)
  showCharts = if(length(l)==1 & is.list(l)) l[[1]] else l
  
  #get the divs for each of the charts as a string
  #this will be used by whisker.render later
  chartDivs <- paste(
    sapply(
      showCharts,function(rCh){
        return(paste(capture.output(rCh$print()),collapse="\n"))
      }
    ),
    collapse = "\n"
  )
  
  viewer = getOption("viewer")  #if not null then use RStudio viewer
  
  #if viewer is not null then 
  #we will need to either use http assets or copy js and css into same directory
  if (!grepl("^http", showCharts[[1]]$LIB$url) && !is.null(viewer)) {
    temp_dir = tempfile(pattern = 'rCharts')
    dir.create(temp_dir)
    suppressMessages(copy_dir_(
      showCharts[[1]]$LIB$url,
      file.path(temp_dir,showCharts[[1]]$LIB$name)
    ))
    tf <- file.path(temp_dir, "index.html")
    
    #get css and script files to add into head
    #will need to copy these files in directory to use with RStudio Viewer
    assets = get_assets(showCharts[[1]]$LIB, static = F, cdn = F)
    
    cat(
      whisker::whisker.render(
        readLines(
          system.file(
            "/libraries/dygraph/layouts/multi.html",
            package = "rChartsDygraphs")
        )
      ),
      file = tf)

    viewer(tf)
  } else {
    #if not using RStudio Viewer can use assets in rChartsDygraphs directory
    #or if using RStudio Viewer and non local (http assets)
    #  can use those without copying
    assets = get_assets(showCharts[[1]]$LIB, static = T, cdn = F)
    
    cat(
      whisker::whisker.render(
        readLines(
          system.file(
            "/libraries/dygraph/layouts/multi.html",
            package = "rChartsDygraphs")
        )
      ),
      file = tf <- tempfile(fileext = ".html")
    )
    if (!is.null(viewer)) {
      viewer(tf)
    } else {
      browseURL(tf)
    }
  }
}

#' Just a copy of rCharts::get_lib
#' 
#' Copied to rChartsDygraphs package namespace, for Dygraph$new() to initialize lib field properly 
get_lib <- function(lib){
  if (grepl("^http", lib)){
    return(list(name = basename(lib), url = lib))
  }
  if (file.exists(lib)){
    lib_url <- normalizePath(lib)
    lib <- basename(lib_url)
  } else {
    lib_url <- system.file('libraries', lib, package = 'rChartsDygraphs')
  }
  return(list(name = basename(lib), url = lib_url))
}

#' Just a copy of rCharts::add_lib_assets
#' 
#' Copied to rChartsDygraphs package namespace, so that it calls rChartsDygraphs::get_lib,
#' not rCharts::get_lib
add_lib_assets <- function(lib, cdn = F){
  assets = get_assets(get_lib(lib), cdn = cdn)
  styles <- lapply(assets$css, function(style){
    sprintf("<link rel='stylesheet' href='%s'>", style)
  })
  scripts <- lapply(assets$jshead, function(script){
    sprintf("<script type='text/javascript' src='%s'></script>", script)
  })
  paste(c(styles, scripts), collapse = '\n')
}
