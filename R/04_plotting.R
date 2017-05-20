#------------------------------------------------------------------------------
#' @title Plot simulation results from csv files
#' @description This function accepts multiple csv files as input, and plots history match plots using ggplot
#' @param basefile The full path (relative or absolute) to a csv file with data to be used to compare against multiple cases, e.g the history file to be compared with multiple model runs.
#' @param compfiles A list of csv files with data to be compared against the base file.
#' @param template A file describing which data is tobe used in the comparison plots
#' @param outdir Where the output plots should be stored.
#' @export
#plotflow <- function(basefile, compfiles, template, outdir="."){
plotcase <- function(casedata){
  ggp <- ggplot2::ggplot(data=casedata,
                         ggplot2::aes(x=DATE, y= VALUE, color = KEYWORD))
  ggp <- ggp + ggplot2::geom_line()
  ggp <- ggp + ggplot2::facet_grid(KEYWORD ~ WGNAME)
  ggp
}
#------------------------------------------------------------------------------
#' @export
.uniquevars <- function(long){
  vl <- unique(paste(long$CASENAME,long$WGNAME,long$KEYWORD,sep=":"))
  vldf <- as.data.frame(t(as.data.frame(strsplit(vl,":"))),
                        stringsAsFactors = FALSE)
  rownames(vldf) <- NULL
  rownames(vldf) <- rownames(vldf, do.NULL = FALSE, prefix="")
  colnames(vldf) <- c("CASENAME","WGNAME","KEYWORD")
  return(vldf)
}
#------------------------------------------------------------------------------
#' @title Create individual plot for each name and keyword in each case
#' @description This function accepts a long dataframe as input, and creates plots using ggplot
#' @param long A long format summary dataframe
#' @param casenames A list with casenames to be plotted
#' @param wgnames A list with well/group/filed names to be plotted
#' @param keywords A list with parameters to be plotted, e.g. "WOPR"
#' @param ncolumns How many columns of plots to display
#' @export
ploteach <- function(long,
                     casenames = NULL,
                     wgnames = NULL,
                     keywords = NULL,
                     ncolumns = 3){
  # all of the combinations of case, we3ll and parameter with data
  vldf <- .uniquevars(long)
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
  np <- length(vldf$CASENAME)
  if(np < 1){
    warning("No data selected for plotting.")
    np <- 1
  }
  nc <- ncolumns
  nr <- ceiling(np / nc)
  op <- par(mfrow = c(as.integer(nr),as.integer(nc)))
  # plot the selected data by well and parameter, colored by case
  for(i in 1:np){
    filt <-vldf$WGNAME[i] == long$WGNAME &
      vldf$KEYWORD[i] == long$KEYWORD
    plotdf <- long[filt,]
    ggp <- ggplot2::ggplot(data=plotdf,
                           ggplot2::aes(x=DATE, y= VALUE, color = CASENAME))
    ggp <- ggp + ggplot2::geom_line()
    ggp <- ggp + ggplot2::labs(
      title = paste("Well:  ", vldf$WGNAME[i], sep = " "),
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
      legend.position = c(0.99, 0.5)
    )
    print(ggp)
  }
  par(op)
}
#------------------------------------------------------------------------------
