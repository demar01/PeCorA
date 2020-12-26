#' scale_by
#'
#' @param object NULL
#' @param data NULL
#' @param scale NULL
#' @examples
#' if (interactive()) {
#'   dat <- data.frame(
#'   f1 = rep(c("a", "b", "c"), c(5, 10, 20)),
#'   x1 = rnorm(35, rep(c(1, 2, 3), c(5, 10, 20)),
#'   rep(c(.5, 1.5, 3), c(5, 10, 20))))
#'   dat$x1_scaled_by_f1 <- scale_by(x1 ~ f1, dat)
#' }
#' @return center and scales a numeric variable within each level of a factor
#' (or the interaction of several factors
#' @export
scale_by <- function(object = NULL, data = NULL, scale = 1) {

  if (inherits(object, "scaledby")) {
    pred <- attr(object, "pred")
  } else if (inherits(object, "scaledby_pred")) {
    pred <- object
  } else if (inherits(object, "formula")) {
    pred <- NULL
    formula <- object
  } else {
    stop("Invalid 'object'; should be a formula or result of previous",
         " scale_by call")
  }

  if (!is.null(pred)) {
    if (!inherits(pred, "scaledby_pred")) {
      stop("'object', if not a formula, must be the 'pred' attribute of a\n",
           " previous scale_by call, or the numeric variable returned\n",
           " by a previous scale_by call")
    }
    formula <- pred$formula
  }

  if (is.null(data)) {
    environment(formula) <- parent.frame()
    data <- stats::model.frame(formula = formula, na.action = "na.pass")
  } else {
    data <- stats::model.frame(formula = formula, data = data,
                               na.action = "na.pass")
  }

  mt <- attr(data, "terms")
  fmat <- attr(mt, "factors")
  if (!is.matrix(fmat) || sum(rowSums(fmat) == 0) != 1 ||
      all(rowSums(fmat) == 0)) {
    stop("'formula' must be a two-sided formula")
  }

  x <- data[[1]]
  if (!is.numeric(x)) stop("left hand side of 'formula' must be numeric")
  if (is.integer(x)) x[] <- as.numeric(x[])
  a <- attributes(x)
  a$class <- class(x)
  if (!is.matrix(x)) x <- matrix(x)
  d <- ncol(x)

  facs <- rownames(fmat)[rowSums(fmat) > 0]
  data[facs] <- lapply(data[facs], function(f) addNA(factor(f, ordered = FALSE)))
  fi <- interaction(data[facs], drop = TRUE)
  levels(fi)[is.na(levels(fi))] <- "NA"

  if (!is.null(pred)) {
    centers <- pred$centers
    scales <- pred$scales
    new_center <- pred$new_center
    new_scale <- pred$new_scale
    scale <- 1 * is.matrix(scales)
    if (ncol(centers) != d || (scale && ncol(scales) != d)) {
      stop("'pred' and 'data' imply different dimensions for numeric variable")
    }

    if (length(newlvs <- setdiff(levels(fi), rownames(centers)))) {
      newc <- matrix(new_center, length(newlvs), d, byrow = TRUE)
      rownames(newc) <- newlvs
      centers <- rbind(centers, newc)
      if (scale) {
        news <- matrix(new_scale, length(newlvs), d, byrow = TRUE)
        rownames(news) <- newlvs
        scales <- rbind(scales, news)
      }
    }

    fi <- factor(fi, levels = rownames(centers))

  } else {
    if (!is.numeric(scale) || !is.vector(scale) || any(scale < 0)) {
      stop("'scale' must be a positive numeric vector or '0' to indicate ",
           "centering only")
    }
    if (any(scale == 0)) scale <- 0
    if (length(scale) == 1) scale <- rep(scale, d)
    if (length(scale) != d) {
      stop("'scale' must have either a single element, or as many elements ",
           "as there are columns in the numeric variable")
    }

    xi <- msplit(x, fi)
    r2 <- sapply(xi, nval) >= 2
    xi <- xi[r2]

    centers <- matrix(nrow = nlevels(fi), ncol = d)
    rownames(centers) <- levels(fi)
    if (d > 1) {
      centers[r2, ] <- t(sapply(xi, colMeans))
    } else {
      centers[r2, ] <- matrix(sapply(xi, colMeans))
    }
    if (all(is.na(centers))) {
      stop("at least one group must have more than one observation with no NAs")
    }
    new_center <- unname(colMeans(centers, na.rm = TRUE))
    if (any(rna <- rowna(centers))) {
      centers[rna, ] <- matrix(new_center, sum(rna), d, byrow = TRUE)
    }

    if (scale[1]) {
      scales <- matrix(nrow = nlevels(fi), ncol = d)
      rownames(scales) <- levels(fi)
      if (d > 1) {
        scales[r2, ] <- t(sapply(xi,
                                 function(i) apply(i, 2, sd, na.rm = TRUE) / scale))
      } else {
        scales[r2, ] <- matrix(sapply(xi,
                                      function(i) apply(i, 2, sd, na.rm = TRUE) / scale))
      }
      new_scale <- unname(colMeans(scales, na.rm = TRUE))
      if (any(rna)) {
        scales[rna, ] <- matrix(new_scale, sum(rna), d, byrow = TRUE)
      }
    } else {
      scales <- 0
      new_scale <- 0
    }
  }

  fi <- as.numeric(fi)
  if (scale[1]) {
    x <- (x - centers[fi, , drop = FALSE]) / scales[fi, , drop = FALSE]
  } else {
    x <- x - centers[fi, , drop = FALSE]
  }

  if (ncol(x) == 1) {
    x <- as.vector(x)
  }
  a$pred <- structure(list(formula = formula, centers = centers, scales = scales,
                           new_center = new_center, new_scale = new_scale), class = c("scaledby_pred",
                                                                                      "list"))
  a$class <- unique(c("scaledby", a$class))
  attributes(x) <- a
  return(x)
}

msplit <- function(x, f, byrow = TRUE, drop = FALSE, ...) {
  if (byrow) {
    return(lapply(split(x = seq_len(nrow(x)), f = f, drop = drop, ...),
                  function(ind) x[ind, , drop = FALSE]))}}

rowna <- function(x, f = any) {
  if (length(dim(x)) != 2) stop("'x' must have two dims")
  return(apply(is.na(x), 1, f))
  }

nval <- function(x, rm.na = TRUE) {
  if ((L <- length(dim(x))) > 2) stop("'x' must have at most two dims")
  x <- unique(x)
  if (!L) {
    if (rm.na) return(length(setdiff(x, NA)))
return(length(x))
 }
  if (rm.na) x <- x[!rowna(x), , drop = FALSE]
  return(nrow(x))
 }

