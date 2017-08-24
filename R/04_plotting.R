#------------------------------------------------------------------------------
#' @title Create an individual plot for each name(WELL/FIELD/GROUP) and keyword, with multiple cases on each plot.
#' @description This function accepts a long format dataframe as input, and creates plots using ggplot
#' @param longdata A long format summary dataframe, not the csv file stored with each simulation case output.  This may be either the output of the eclsum function, or the file "REPORTS/PROJSUM.csv".
#' @param casenames A list with casenames to be plotted.  Default is all casenames in the dataframe.
#' @param wgnames A list with well/group/field names to be plotted.  Default is all WELL/FIELD/GROUP names in the dataframe.
#' @param keywords A list with parameters to be plotted, e.g. "WOPR".  Default is all keywords in the dataframe.
#' @param basecase The case against which all others will be compared.
#' @param ncolumns How many columns of plots to display.  The default is a display 3 columns wide, with as many rows as necessary to plot all of the desired plots.
#' @details The intent of this function is to compare multiple runs (i.e. "cases") on the same plot.  With too many cases, the plot will quickly become illegible, so a reasonable selection should be made.
#' @export
PlotEach <- function(longdata,
                     casenames = NULL,
                     wgnames = NULL,
                     keywords = NULL,
                     basecase = NULL,
                     ncolumns = 3){
  # all of the combinations of case, well and parameter with data
  allnames <- casenames
  if (!is.null(basecase)) {allnames <- c(casenames, basecase)}
  vars.long.df <- .SelectPlotVars(longdata, casenames = allnames,
                                  wgnames = wgnames, keywords = keywords)
  all.plots.df <- .FilterLong(longdata, vars.long.df)
  plot.vars.df <- unique(vars.long.df[,c("WGNAME", "KEYWORD")])
  np <- nrow(plot.vars.df)
  if (np < 1) {
    return(warning("No data selected for plotting."))
  }
  # plot layout parameters
  nc <- ncolumns
  nr <- ceiling(np / nc)
  op <- graphics::par(mfrow = c(as.integer(nr),as.integer(nc)))
  # plot the selected data by well and parameter, colored by case
  for (i in 1:np) {
    filt <- plot.vars.df$WGNAME[i] == all.plots.df$WGNAME &
      plot.vars.df$KEYWORD[i] == all.plots.df$KEYWORD
    plotdf <- all.plots.df[filt,]
    plotdf$CASENAME <- factor(plotdf$CASENAME, levels = unique(plotdf$CASENAME))
    rownums <- match(unique(plotdf$CASENAME), plotdf$CASENAME)
    plotdf <- dplyr::mutate(plotdf, linetype = "solid", symbol = 20)
    # 20 is a small filled circle
    plotdf$linetype[plotdf$CASENAME == basecase] <- "blank"
    plotdf$symbol[!plotdf$CASENAME == basecase] <- 46
    #  46 is a dot (invisible under a line)
    lns_pts <- data.frame(CASENAME = plotdf$CASENAME[rownums],
                              linetype = plotdf$linetype[rownums],
                              symbol = plotdf$symbol[rownums],
                              row.names = plotdf$CASENAME[rownums],
                              stringsAsFactors = FALSE)
#    man.cols <- iwanthue(nrow(lns_pts))
    man.cols <- pals::glasbey(nrow(lns_pts))
#    pals::pal.bands(man.cols)
    n <- plot.vars.df$WGNAME[i]
    k <- plot.vars.df$KEYWORD[i]
    lead <- "Well:  "
    b1pat <- "^\\d+$"
    b2pat <- "^\\d+_\\d+_\\d+$"
    fpat <- "FIELD"
    if (any(grep(b1pat,n,perl = TRUE)) |
        any(grep(b2pat,n,perl = TRUE))) {lead <- "Block:  "}
    if (any(grep(fpat,n,perl = TRUE))) {lead <- ""}
    title <- .WGN_KW2Title(wgn = n, keyword = k)
     ggp <- ggplot2::ggplot(data = plotdf,
                           ggplot2::aes(x = DATE, y = VALUE, color = CASENAME,
                                        order = as.numeric(CASENAME)))
    ggp <- ggp +
      ggplot2::geom_line(data = plotdf[!plotdf$CASENAME == basecase,],
                         ggplot2::aes(x = DATE, y = VALUE, color = CASENAME))
    ggp <- ggp +
      ggplot2::geom_point(data = plotdf[plotdf$CASENAME == basecase,],
                          ggplot2::aes(x = DATE, y = VALUE, color = CASENAME))
    ggp <- ggp +
      ggplot2::scale_color_manual(breaks = lns_pts$CASENAME,
                                  values = man.cols,
                                  guide = ggplot2::guide_legend(
                                    override.aes = list(shape = lns_pts$symbol,
                                                        linetype = lns_pts$linetype)
                                  ))
     ggp <- ggp + ggplot2::labs(
      title = title,
      y = .KW2Label(plot.vars.df$KEYWORD[i])
    )
    ggp <- ggp + ggplot2::theme_bw()
    ggp <- ggp + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 7),
      legend.background = ggplot2::element_rect(fill = "white",
                                                color = "black"),
      legend.spacing = grid::unit(0.1, units = "points"),
      legend.justification = "right",
      legend.position = c(0.99, 0.5),
      panel.grid.major = ggplot2::element_line(color = "lightgrey",
                                               size = 1,
                                               linetype = 1)
    )
#    ggp
    print(ggp)
  }
  graphics::par(op)
}
#------------------------------------------------------------------------------
.SelectPlotVars <- function(longdata,
                             casenames = NULL,
                             wgnames = NULL,
                             keywords = NULL){
  vars.long.df = unique(longdata[,c("CASENAME", "WGNAME", "KEYWORD")])
  df <- vars.long.df[0,]
  # filter down to selected cases
  if (!is.null(casenames)) {
    for (i in 1:length(casenames)) {
      filt <- vars.long.df$CASENAME == casenames[i]
      df <- rbind(df, vars.long.df[filt,])
    }
    vars.long.df <- df
  }
  df <- vars.long.df[0,]
  # filter down again to selected wells
  if (!is.null(wgnames)) {
    for (i in 1:length(wgnames)) {
      filt <- vars.long.df$WGNAME == wgnames[i]
      df <- rbind(df, vars.long.df[filt,])
    }
    vars.long.df <- df
  }
  df <- vars.long.df[0,]
  # filter down again to selected parameters
  if (!is.null(keywords)) {
    for (i in 1:length(keywords)) {
      filt <- vars.long.df$KEYWORD == keywords[i]
      df <- rbind(df, vars.long.df[filt,])
    }
    vars.long.df <- df
  }
  return(vars.long.df)
}
#------------------------------------------------------------------------------
.FilterLong <- function(longdata, vars.long.df){
  case_filt <- .List2Filt(vars.long.df$CASENAME, longdata$CASENAME)
  wgn_filt <- .List2Filt(vars.long.df$WGNAME, longdata$WGNAME)
  key_filt <- .List2Filt(vars.long.df$KEYWORD, longdata$KEYWORD)
  filt <- case_filt & wgn_filt & key_filt
  all.plots.df <- longdata[filt,]
  return(all.plots.df)
}
#------------------------------------------------------------------------------
.BuildPLotFIlter <- function(longdata,
                             casenames = NULL,
                             wgnames = NULL,
                             keywords = NULL,
                             vars.long.df = NULL){
  df <- vars.long.df[0,]
  # filter down to selected cases
  if (!is.null(casenames)) {
    for (i in 1:length(casenames)) {
      filt <- vars.long.df$CASENAME == casenames[i]
      df <- rbind(df, vars.long.df[filt,])
    }
    vars.long.df <- df
  }
  # filter down again to selected wells
  if (!is.null(wgnames)) {
    for (i in 1:length(wgnames)) {
      filt <- vars.long.df$WGNAME == wgnames[i]
      df <- rbind(df, vars.long.df[filt,])
    }
    vars.long.df <- df
  }
  # filter down again to selected parameters
  if (!is.null(keywords)) {
    for (i in 1:length(keywords)) {
      filt <- vars.long.df$KEYWORD == keywords[i]
      df <- rbind(df, vars.long.df[filt,])
    }
    vars.long.df <- df
  }
  return(vars.long.df)
}
#------------------------------------------------------------------------------
.UniqueVars <- function(longdata){
  vl <- unique(paste(longdata$CASENAME,
                     longdata$WGNAME,
                     longdata$KEYWORD,sep = ":"))
  vars.long.df <- as.data.frame(t(as.data.frame(strsplit(vl,":"))),
                                stringsAsFactors = FALSE)
  rownames(vars.long.df) <- NULL
  rownames(vars.long.df) <- rownames(vars.long.df, do.NULL = FALSE,
                                     prefix = "")
  colnames(vars.long.df) <- c("CASENAME","WGNAME","KEYWORD")
  return(vars.long.df)
}
#------------------------------------------------------------------------------
