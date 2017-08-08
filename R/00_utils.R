#' @title A substitute for the DiceDesign::rss2d to fix a bug:  2D graphical tool for defect detection of Space-Filling Designs.
#' @description For a 2-dimensional design, the 2D radial scanning statistic (RSS) scans angularly the domain. In each direction, it compares the distribution of projected points to their theoretical distribution under the assumption that all design points are drawn from uniform distribution. For a d-dimensional design, all pairs of dimensions are scanned.The RSS detects the defects of low discrepancy sequences or orthogonal arrays, and can be used for selecting space-filling designs.
#' @param design a matrix or data.frame containing the d-dimensional design of experiments. The row no. i contains the values of the d input variables corresponding to simulation no. i
#' @param lower the domain lower boundaries.
#' @param upper the domain upper boundaries.
#' @param gof.test.type an optional character indicating the kind of statistical test to be used to test the goodness-of-fit of the design projections to their theoretical distribution. Several tests are available, see unif.test.statistic. Default is "greenwood".
#' @param gof.test.stat an optional number equal to the goodness-of-fit statistic at level 5\%. Default is the modified test statistic for fully specified distribution (see details below).
#' @param transform an optional character indicating what type of transformation should be applied before testing uniformity. Only one choice available "spacings", that lead to over-detection. Default - and recommended - is NULL.
#' @param n.angle an optional number indicating the number of angles used. Default is 360 corresponding to a 0.5-degree discretization step. Note that the RSS curve is continuous.
#' @param graphics an optional integer indicating whether a graph should be produced. If negative, no graph is produced. If superior to 2, the RSS curve only is plotted in the worst 2D coordinate subspace (corr. to the worst value of statistic). If 1 (default), the design is also added, with its projections onto the worst (oblique) axis.
#' @param trace an optional boolean. Turn it to FALSE if you want no verbosity.
#' @param lines.lwd optional number specifying the width of the straight lines involved in the graphical outputs (axis and projections)
#' @param lines.lty optional character string specifying the type of the straight lines involved in the graphical outputs (axis and projections)
#' @param ... optional graphical parameters of plot function, to draw the RSS curve.
#' @return a list with components:
#' @export
rss2d <- function(design, lower, upper, gof.test.type="greenwood", gof.test.stat=NULL, transform=NULL, n.angle=360, graphics=1, trace=TRUE, lines.lwd=1, lines.lty="dotted", ...)  {

  design <- as.matrix(design)
  n <- dim(design)[1]
  d <- dim(design)[2]


  # some arguments checks

  if (!is.element(gof.test.type, c("greenwood", "qm", "ks", "V", "cvm"))) stop("The goodness-of-fit test must be one of: Greenwood, Quesenberry-Miller, Kolmogorov-Smirnov, V = (D+) +  (D-), or Cramer-Von Mises")

  if ((length(lower)!=d) & (length(upper)!=d)) stop("lower and upper must be d-dimensional vectors")


  # domain transformation to [-1,1]^d

  for (j in 1:d) {
    design.col <- design[,j]
    design.col.min <- min(design.col)
    if (design.col.min < lower[j]) stop('the minimum of design values is not compatible with lower')
    design.col.max <- max(design.col)
    if (design.col.max > upper[j]) stop('the maximum of design values is not compatible with upper')
    design.col <- 2*((design.col - lower[j])/(upper[j] - lower[j]) - 0.5)
    design[,j] <- design.col
  }


  # compute the subdivision of the (half) circle in cartesian coordinates

  theta.degree <- seq(from=0, to=180, length=n.angle+1)
  theta.degree <- theta.degree[1:n.angle]
  theta.degree <- as.matrix(theta.degree)
  theta <- theta.degree*2*pi/360
  cos.theta <- cos(theta)
  sin.theta <- sin(theta)
  n.theta <- length(theta)
  subdiv.halfcircle <- cbind(cos.theta, sin.theta)


  # loop over all pairs of dimensions

  global.stat <- matrix(NA, d, d)
  global.stat.max <- 0

  if (trace) {
    cat("\n2D Radial Scanning Statistic (RSS) with ", toupper(gof.test.type), " statistic\n", sep="")
    cat("Discretization step (in degree) : ", 180/n.angle, sep="")
    cat("\n\nMaximum of RS statistic values (global statistic) per pair of dimensions")
  }


  print.out <- data.frame(global.stat = rep(NA, (d*(d-1))%/%2))
  meter <- 0

  for (i in 1:(d-1)) {
    x <- design[,i]

    for (j in ((i+1):d)) {
      y <- design[,j]

      # compute anglewise statistic


      # 1st step : compute the matrix of the F(projected points onto Vect(cos.theta, sin.theta))
      out <- .C("C_rss2Dloop", as.double(x), as.double(y), as.double(cos.theta), as.double(sin.theta), as.integer(n), as.integer(n.theta), ans=double(n * n.theta), PACKAGE="runOPM")
      F.projections <- matrix(out$ans, n, n.theta)

      # 2nd step : for each angle, compute the statistic
      # In future version, should be computed inside the C loop
      anglewise.stat.ij <- matrix(NA, n.theta, 1)
      for (angle in 1:n.theta) {
        anglewise.stat.ij[angle] <- DiceDesign::unif.test.statistic(x=F.projections[,angle], type=gof.test.type, transform=transform)
      }


      # compute the worst value over all angles and store it
      global.stat.ij <- max(anglewise.stat.ij)
      global.stat[i,j] <- global.stat[j,i] <- global.stat.ij

      if (global.stat.ij > global.stat.max) {
        global.stat.max <- global.stat.ij
        pair.worst <- c(i,j)
        anglewise.stat <- anglewise.stat.ij
      }

      meter <- meter + 1
      print.out[meter, 1] <- global.stat.ij
      name.current <- paste("(", i, ",", j, ")", sep="")
      row.names(print.out)[meter] <- name.current
      if (trace) cat("\n", name.current, " ", global.stat.ij, sep="")

    } # end loop j
  } # end loop i

  if (trace) cat("\n\n")


  # rss curve
  rss.curve.x <- anglewise.stat*subdiv.halfcircle[,1]
  rss.curve.y <- anglewise.stat*subdiv.halfcircle[,2]


  # statistic upper tail percentage points
  # see D'Agostino and Stephens "Goodness-of-fit techniques", 1986

  if (is.null(gof.test.stat)) {
    gof.test.stat <- DiceDesign::unif.test.quantile(type=gof.test.type, n=n, alpha=0.05)
  }

  anglewise.stat.max <- max(anglewise.stat)
  index.max <- which.max(anglewise.stat)
  cos.theta.max <- subdiv.halfcircle[index.max, 1]
  sin.theta.max <- subdiv.halfcircle[index.max, 2]
  dir.max <- c(cos.theta.max, sin.theta.max)

  # --------------------
  # graphics if required
  # --------------------

  if (graphics>=0) {

    design.names <- names(as.data.frame(design))

    design <- design[ , pair.worst]
    design.names <- design.names[pair.worst]


    if (is.element(graphics, c(0,1))) {
      graphics::par(mfrow=c(1,2+graphics))
      graphics::plot(design, xlim=c(-1,1), ylim=c(-1,1),
           xlab=design.names[1], ylab=design.names[2])
    }

    # draw the rss curve
    if (graphics>0) {
      rx <- c(rss.curve.x, -rss.curve.x, rss.curve.x[1])
      ry <- c(rss.curve.y, -rss.curve.y, rss.curve.y[1])
      graph.size <- max(abs((anglewise.stat.max)*dir.max), gof.test.stat)*1.05
      graphics::plot(rx, ry, xlim=c(-graph.size,graph.size), ylim=c(-graph.size, graph.size),
           xlab="", ylab="", ...)

      # draw the circle with radius equal to the threshold at significance level 5%
      theta_aux <- seq(from=0, to=2*pi+0.1,by=0.1)
      graphics::lines(gof.test.stat*cos(theta_aux), gof.test.stat*sin(theta_aux))

      # draw the coordinate axis in dotted lines
      graphics::abline(h=0, v=0, lty=lines.lty, col="black", lwd=lines.lwd)
    }

    if (is.element(graphics, c(0,1))) {
      graphics::plot(design, xlim=c(-1,1), ylim=c(-1,1),
           xlab=design.names[1], ylab=design.names[2])
      projections <- design %*% dir.max
      graphics::points(projections*dir.max[1], projections*dir.max[2], pch=20, col="red")
      if (cos.theta.max==0) {
        graphics::lines(c(0,0), c(-1,1), col="red")
      } else graphics::lines(c(-1,1), c(-1,1)*sin.theta.max/cos.theta.max, col="red")
      for (i in 1:n) graphics::lines(c(design[i,1], projections[i]*cos.theta.max), c(design[i,2], projections[i]*sin.theta.max), lty=lines.lty, lwd=lines.lwd)
    }

    graphics::par(mfrow=c(1,1))
  }

  return(list(global.stat=global.stat, worst.case=pair.worst, worst.dir=dir.max, stat=as.numeric(anglewise.stat), angle=as.numeric(theta), curve=cbind(c(rss.curve.x,-rss.curve.x), c(rss.curve.y,-rss.curve.y)),  gof.test.stat=gof.test.stat))
}

#' @title A substitute for the DiceDesign::rss3d to fix a bug
#' @description  For a 3-dimensional design, the 3D radial scanning statistic (RSS) scans angularly the domain. In each direction, it compares the distribution of projected points to their theoretical distribution under the assumption that all design points are drawn from uniform distribution. For a d-dimensional design, all triplets of dimensions are scanned. The RSS detects the defects of low discrepancy sequences or orthogonal arrays, and can be used for selecting space-filling designs.
#' @param design a matrix or data.frame containing the d-dimensional design of experiments. The row no. i contains the values of the d input variables corresponding to simulation no. i
#' @param lower the domain lower boundaries.
#' @param upper the domain upper boundaries.
#' @param gof.test.type an optional character indicating the kind of statistical test to be used to test the goodness-of-fit of the design projections to their theoretical distribution. Several tests are available, see unif.test.statistic. Default is "greenwood".
#' @param gof.test.stat an optional number equal to the goodness-of-fit statistic at level 5\%. Default is the modified test statistic for fully specified distribution (see details below).
#' @param transform an optional character indicating what type of transformation should be applied before testing uniformity. Only one choice available "spacings", that lead to over-detection. Default - and recommended - is NULL.
#' @param n.angle an optional number indicating the number of angles used. Default is 360 corresponding to a 0.5-degree discretization step. Note that the RSS curve is continuous.
#' @param graphics an optional integer indicating whether a graph should be produced. If negative, no graph is produced. Otherwise (default), the design is plotted in the worst 3D coordinate subspace (corr. to the worst value of statistic), with its projections onto the worst (oblique) axis
#' @param trace an optional boolean. Turn it to FALSE if you want no verbosity.
#' @return a list with components:
#' @export
rss3d<-function(design, lower, upper, gof.test.type="greenwood", gof.test.stat=NULL, transform=NULL, n.angle=60, graphics=1, trace=TRUE){


  design <- as.matrix(design)
  n <- dim(design)[1]
  d <- dim(design)[2]

  #if (ncol(design)!=3) stop("'design' must have 3 columns")
  #if ((length(lower)!=3) & (length(upper)!=3)) stop("lower and upper must be 3-dimensional vectors")
  #dimension <- 3

  # some arguments checks

  if (d < 3) stop("rss3d is for d-dimensional designs with d >= 3")
  if (!is.element(gof.test.type, c("greenwood", "qm", "ks", "V", "cvm"))) stop("The goodness-of-fit test must be one of: Greenwood, Quesenberry-Miller, Kolmogorov-Smirnov, V = (D+) +  (D-), or Cramer-Von Mises")

  if ((length(lower)!=d) & (length(upper)!=d)) stop("lower and upper must be d-dimensional vectors")


  # domain transformation to [-1,1]^d

  for (j in 1:d) {
    design.col <- design[,j]
    design.col.min <- min(design.col)
    if (design.col.min < lower[j]) stop('the minimum of design values is not compatible with lower')
    design.col.max <- max(design.col)
    if (design.col.max > upper[j]) stop('the maximum of design values is not compatible with upper')
    design.col <- 2*((design.col - lower[j])/(upper[j] - lower[j]) - 0.5)
    design[,j] <- design.col
  }


  # angles definition

  theta.degree <- seq(from=0, to=180, length=n.angle+1)
  theta.degree <- theta.degree[1:n.angle]
  theta.degree <- as.matrix(theta.degree)
  theta <- theta.degree*2*pi/360
  n.theta <- length(theta)
  cos.theta <- cos(theta); sin.theta <- sin(theta)

  phi.degree <- seq(from=-90, to=90, length=n.angle+10)
  phi.degree <- phi.degree[1:n.angle]
  phi.degree <- as.matrix(phi.degree)
  phi <- phi.degree*2*pi/360
  n.phi <- length(phi)
  cos.phi <- cos(phi); sin.phi <- sin(phi)


  # Loops on theta and phi to build the matrix of statistic values, one of which is implemented in C

  ax <- cos.theta%*%t(cos.phi)
  ay <- sin.theta%*%t(cos.phi)
  az <- rep(1, n.theta)%*%t(sin.phi)

  print.out <- data.frame(global.stat = rep(NA, (d*(d-1)*(d-2))%/%6))

  # loop over all triplets of dimensions

  anglewise.stat.max <- anglewise.stat <- matrix(0, n.theta, n.phi)
  global.stat.array <- array(NA, c(d, d, d))
  global.stat.max <- 0
  meter <- 0

  if (trace) {
    cat("\n3D Radial Scanning Statistic (RSS) with ", toupper(gof.test.type), " statistic\n", sep="")
    cat("Discretization step (in degree) : ", 180/n.angle, sep="")
    cat("\n\nMaximum of RS statistic values (global statistic) per triplet of dimensions")
  }

  for (i1 in 1:(d-2)) {
    x <- design[, i1]

    for (i2 in ((i1+1):(d-1))) {
      y <- design[, i2]

      for (i3 in ((i2+1):d)) {
        z <- design[, i3]


        for (j in 1:n.phi){

          # 1st step : compute the matrix of the F(projected points onto Vect(cos.theta, sin.theta))

          out <- .C("C_rss3Dloop", as.double(x), as.double(y), as.double(z), as.double(ax), as.double(ay), as.double(az), as.integer(n), as.integer(n.theta), as.integer(n.phi), as.integer(j-1), ans=double(n * n.theta), PACKAGE="runOPM")

          F.projections <- matrix(out$ans, n, n.theta)


          # 2nd step: test statistic computations

          for (k in 1:n.theta) {
            anglewise.stat[k,j] <- DiceDesign::unif.test.statistic(x=F.projections[,k], type=gof.test.type, transform=transform)
          }	# end loop over theta (k)

        } # end loop over phi (j)

        # compute the worst value over all angles and store it
        global.stat <- max(anglewise.stat)
        global.stat.array[i1,i2,i3] <- global.stat

        if (global.stat > global.stat.max) {
          global.stat.max <- global.stat
          triplet.worst <- c(i1,i2,i3)
          anglewise.stat.max <- anglewise.stat
        }

        meter <- meter + 1
        print.out[meter, 1] <- global.stat
        name.current <- paste("(", i1, ",", i2, ",", i3, ")", sep="")
        row.names(print.out)[meter] <- name.current
        if (trace) cat("\n", name.current, " ", global.stat, sep="")

      } # end loop i3
    } # end loop i2
  } # end loop i1

  if (trace) cat("\n\n")

  # threshold at significance level 5%

  if (is.null(gof.test.stat)) {
    gof.test.stat <- DiceDesign::unif.test.quantile(type=gof.test.type, n=n, alpha=0.05)
  }


  # 3D graph with package rgl ##

  index.max <- which.max(anglewise.stat.max)
  ax.max <- ax[index.max]
  ay.max <- ay[index.max]
  az.max <- az[index.max]
  dir.max <- c(ax.max, ay.max, az.max)

  if (graphics > 0) {

    if (requireNamespace("rgl", quietly = TRUE)) {
      rgl::open3d()
      design <- design[, triplet.worst]
      x <- design[, 1]
      y <- design[, 2]
      z <- design[, 3]

      phi.max <- theta.max <- NA
      for (j in 1:n.phi) {
        for (k in 1:n.theta) {
          if (abs(anglewise.stat.max[k,j]-global.stat.max)<1e-10) {
            phi.max <- phi[j]
            theta.max <- theta[k]
          }
        }
      }

      projections <- as.matrix(design) %*% dir.max

      rgl::plot3d(x, y, z, size=5, col="blue", xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), xlab="", ylab="", zlab="")

      a.max <- max(abs(dir.max))

      rgl::plot3d(c(-1,1)*ax.max/a.max, c(-1,1)*ay.max/a.max, c(-1,1)*az.max/a.max, col="red", type="l", size=2, add=TRUE)


      for (i in 1:n) {
        h <- projections[i]*dir.max - design[i,]
        if (max(abs(h)) > 1e-8) {
          lambda <- min(min((sign(h) - design[i,])/h), 1)
          rgl::plot3d(c(design[i,1], design[i,1] + lambda*h[1]), c(design[i,2], design[i,2] + lambda*h[2]), c(design[i,3], design[i,3] + lambda*h[3]), type="l", col="red", add=TRUE)
        }
        if (max(abs(projections[i]*dir.max))<=1) rgl::plot3d(projections[i]*dir.max[1], projections[i]*dir.max[2], projections[i]*dir.max[3], pch=20, col="red", size=5, add=TRUE)
      }
      graphics::par(mfrow=c(1,1))

    } else {
      print("Error : the package rgl is not installed")
    }


  } # end of conditional block: "if graphics > 0"

  return(list(stat=anglewise.stat.max, angle=data.frame(theta=theta, phi=phi), global.stat=global.stat.array, print.out=print.out, gof.test.stat=gof.test.stat, worst.case=triplet.worst, worst.dir=dir.max))

}

#==============================================================================
# from https://github.com/johnbaums/hues
#
#' Generate a colour palette by k-means clustering of LAB colour space.
#'
#' Generate a palette of distinct colours through k-means clustering of LAB
#' colour space.
#'
#' @param n Numeric. The number of colours to generate.
#' @param hmin Numeric, in the range [0, 360]. The lower limit of the hue range
#'   to be clustered.
#' @param hmax Numeric, in the range [0, 360]. The upper limit of the hue range
#'   to be clustered.
#' @param cmin Numeric, in the range [0, 180]. The lower limit of the chroma
#'   range to be clustered.
#' @param cmax Numeric, in the range [0, 180]. The upper limit of the chroma
#'   range to be clustered.
#' @param lmin Numeric, in the range [0, 100]. The lower limit of the luminance
#'   range to be clustered.
#' @param lmax Numeric, in the range [0, 100]. The upper limit of the luminance
#'   range to be clustered.
#' @param plot Logical. Should the colour swatches be plotted (using
#'   \code{\link{swatch}})?
#' @param random Logical. If \code{TRUE}, clustering will be determined by the
#'   existing RNG state. If \code{FALSE}, the seed will be set to \code{1} for
#'   clustering, and on exit, the function will restore the pre-existing RNG
#'   state.
#' @return A vector of \code{n} colours (as hexadecimal strings), representing
#'   centers of clusters determined through k-means clustering of the LAB colour
#'   space delimited by \code{hmin}, \code{hmax}, \code{cmin}, \code{cmax},
#'   \code{lmin} and \code{lmax}.
#' @details Note that \code{iwanthue} currently doesn't support \code{hmin}
#'   greater than \code{hmax} (which should be allowed, since hue is circular).
#' @references
#' \itemize{
#'   \item \href{http://tools.medialab.sciences-po.fr/iwanthue/}{iwanthue - colors for data scientists}
#'   \item \href{https://github.com/medialab/iwanthue}{iwanthue on
#'   GitHub}
#' }
#' @seealso \code{\link{swatch}}
#' @export
#' @importFrom colorspace LAB hex coords
#' @examples
#' iwanthue(5)
#' iwanthue(5, plot=TRUE)
#' iwanthue(5, 0, 240, 0, 24, 0, 100, plot=TRUE) # shades
#' iwanthue(5, 0, 360, 0, 54, 67, 100, plot=TRUE) # pastel
#' iwanthue(5, 0, 360, 54, 180, 27, 67, plot=TRUE) # pimp
#' iwanthue(5, 0, 360, 36, 180, 13, 73, plot=TRUE) #intense
#' iwanthue(3, 0, 300, 60, 180, 73, 100, plot=TRUE) # fluoro
#' iwanthue(3, 220, 260, 12, 150, 0, 53, plot=TRUE) # blue ocean
#------------------------------------------------------------------------------
iwanthue <- function(n, hmin=0, hmax=360, cmin=0, cmax=180, lmin=0, lmax=100,
                     plot=FALSE, random=FALSE) {
  stopifnot(hmin >= 0, cmin >= 0, lmin >= 0,
            hmax <= 360, cmax <= 180, lmax <= 100,
            hmin <= hmax, cmin <= cmax, lmin <= lmax,
            n > 0)
  if(!random) {
    if (exists(".Random.seed", .GlobalEnv)) {
      old_seed <- .GlobalEnv$.Random.seed
      on.exit(.GlobalEnv$.Random.seed <- old_seed)
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv))
    }
    set.seed(1)
  }
  lab <- LAB(as.matrix(expand.grid(seq(0, 100, 1),
                                   seq(-100, 100, 5),
                                   seq(-110, 100, 5))))
  if (any((hmin != 0 || cmin != 0 || lmin != 0 ||
           hmax != 360 || cmax != 180 || lmax != 100))) {
    hcl <- as(lab, 'polarLUV')
    hcl_coords <- coords(hcl)
    hcl <- hcl[which(hcl_coords[, 'H'] <= hmax & hcl_coords[, 'H'] >= hmin &
                       hcl_coords[, 'C'] <= cmax & hcl_coords[, 'C'] >= cmin &
                       hcl_coords[, 'L'] <= lmax & hcl_coords[, 'L'] >= lmin), ]
    lab <- as(hcl, 'LAB')
  }
  lab <- lab[which(!is.na(hex(lab))), ]
  clus <- kmeans(coords(lab), n, iter.max=50)
  if (isTRUE(plot)) {
    swatch(hex(LAB(clus$centers)))
  }
  hex(LAB(clus$centers))
}
#==============================================================================
# from https://github.com/johnbaums/hues
#
#' Plot colour swatches for a vector of colours
#'
#' Plot named colour swatches for a vector of colours.
#'
#' @param x a vector of colours, specified as: colour names (i.e.
#' colour names returned by \code{colors()}); numeric indices into
#' \code{palette()}, or hexadecimal strings in the form \code{"#RRGGBB"}, where
#' \code{RR}, \code{GG}, and \code{BB} are pairs of hexadecimal digits
#' representing red, green, and blue components, in the range \code{00} to
#' \code{FF}.
#' @return \code{NULL}. The colour swatch is plotted to the active plotting
#'   device.
#' @seealso \code{\link{iwanthue}}
#' @export
#' @examples
#' swatch(colours()[1:10])
#' swatch(1:4)
#' swatch(iwanthue(5))
#------------------------------------------------------------------------------
swatch <- function(x) {
  par(mai=c(0.2, max(strwidth(x, "inch") + 0.4, na.rm = TRUE), 0.2, 0.4))
  barplot(rep(1, length(x)), col=rev(x), space = 0.1, axes=FALSE,
          names.arg=rev(x), cex.names=0.8, horiz=T, las=1)
  return(invisible(NULL))
}
#==============================================================================
