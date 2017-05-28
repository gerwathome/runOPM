# to keep CRAN happy with ggplot
globalVariables(c("DATE", "VALUE", "CASENAME"))

#------------------------------------------------------------------------------
#' @title Create an individual plot for each name(WELL/FIELD/GROUP) and keyword, with multiple cases on each plot.
#' @description This function accepts a long format dataframe as input, and creates plots using ggplot
#' @param longdata A long format summary dataframe, not the csv file stored with each simulation case output.  This may be either the output of the eclsum function, or the file "REPORTS/PROJSUM.csv".
#' @param casenames A list with casenames to be plotted.  Default is all casenames in the dataframe.
#' @param wgnames A list with well/group/filed names to be plotted.  Default is all WELL/FIELD/GROUP names in the dataframe.
#' @param keywords A list with parameters to be plotted, e.g. "WOPR".  Default is all keywords in the dataframe.
#' @param ncolumns How many columns of plots to display.  The default is a display 3 columns wide, with as many rows as necessary to plot all of the desired plots.
#' @details The intent of this function is to compare multiple runs (i.e. "cases") on the same plot.  With too many cases, the plot will quickly become illegible, so a reasonable selection should be made.
#' @export
ploteach <- function(longdata,
                     casenames = NULL,
                     wgnames = NULL,
                     keywords = NULL,
                     ncolumns = 3){
  # all of the combinations of case, well and parameter with data
  vldf <- .uniquevars(longdata)
  df <- vldf[0,]
  # filter down to selected cases
  if(!is.null(casenames)){
    for(i in 1:length(casenames)){
      filt <- vldf$CASENAME == casenames[i]
      df <- rbind(df, vldf[filt,])
    }
    vldf <- df
  }
  # filter down again to selected wells
  if(!is.null(wgnames)){
    for(i in 1:length(wgnames)){
      filt <- vldf$WGNAME == wgnames[i]
      df <- rbind(df, vldf[filt,])
    }
    vldf <- df
  }
  # filter down again to selected parameters
  if(!is.null(keywords)){
    for(i in 1:length(keywords)){
      filt <- vldf$KEYWORD == keywords[i]
      df <- rbind(df, vldf[filt,])
    }
    vldf <- df
  }
  #pics <- unique(paste(vldf$WGNAME,vldf$KEYWORD,sep=":"))
  np <- length(unique(paste(vldf$WGNAME,vldf$KEYWORD,sep=":")))
  if(np < 1){
    warning("No data selected for plotting.")
    np <- 1
  }
  nc <- ncolumns
  nr <- ceiling(np / nc)
  op <- graphics::par(mfrow = c(as.integer(nr),as.integer(nc)))
  # plot the selected data by well and parameter, colored by case
  for(i in 1:np){
    filt <-vldf$WGNAME[i] == longdata$WGNAME &
      vldf$KEYWORD[i] == longdata$KEYWORD
    plotdf <- longdata[filt,]
    n <- vldf$WGNAME[i]
    k <- vldf$KEYWORD[i]
    lead <- "Well:  "
    b1pat <- "^\\d+$"
    b2pat <- "^\\d+_\\d+_\\d+$"
    fpat <- "FIELD"
    if(any(grep(b1pat,n,perl=TRUE)) |
       any(grep(b2pat,n,perl=TRUE))){lead <- "Block:  "}
    if(any(grep(fpat,n,perl=TRUE))){lead <- ""}
    title <- paste0(lead,n,":",k)
    ggp <- ggplot2::ggplot(data=plotdf,
                           ggplot2::aes(x=DATE, y= VALUE, color = CASENAME))
    ggp <- ggp + ggplot2::geom_line()
    ggp <- ggp + ggplot2::labs(
      title = title,
      y = .kw2label(vldf$KEYWORD[i])
    )
    ggp <- ggp + ggplot2::theme_bw()
    ggp <- ggp + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle=90, vjust=0.5),
      legend.title = ggplot2::element_text(size=7),
      legend.text = ggplot2::element_text(size=6),
      legend.background = ggplot2::element_rect(fill = "white",
                                                color = "black"),
      legend.spacing = grid::unit(0.1, units="points"),
      legend.justification = "right",
      legend.position = c(0.99, 0.5),
      panel.grid.major = ggplot2::element_line(color="lightgrey",
                                               size = 1,
                                               linetype = 1)
    )
    print(ggp)
  }
  graphics::par(op)
}
#------------------------------------------------------------------------------
.uniquevars <- function(longdata){
  vl <- unique(paste(longdata$CASENAME,
                     longdata$WGNAME,
                     longdata$KEYWORD,sep=":"))
  vldf <- as.data.frame(t(as.data.frame(strsplit(vl,":"))),
                        stringsAsFactors = FALSE)
  rownames(vldf) <- NULL
  rownames(vldf) <- rownames(vldf, do.NULL = FALSE, prefix="")
  colnames(vldf) <- c("CASENAME","WGNAME","KEYWORD")
  return(vldf)
}
#------------------------------------------------------------------------------
