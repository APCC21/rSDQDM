Escore <-function (x, y, scale.x = FALSE, n.cases = NULL, alpha = 1, method = "cluster")
{

  ################################################################################
  # Multivariate goodness-of-fit scoring function

  # Energy score for assessing the equality of two multivariate samples
  # Székely, G.J., Rizzo, M.L. 2013. Energy statistics: A class of statistics
  #  based on distances. Journal of Statistical Planning and Inference, 143(8),
  #  1249-1272. doi:10.1016/j.jspi.2013.03.018
  # Baringhaus, L., Franz, C. 2004. On a new multivariate two-sample test.
  #  Journal of Multivariate Analysis, 88(1), 190-206.
  #  doi:10.1016/S0047-259X(03)00079-4
    n.x <- nrow(x)
    n.y <- nrow(y)
    if (scale.x) {
        x <- scale(x)
        y <- scale(y, center = attr(x, "scaled:center"), scale = attr(x,
            "scaled:scale"))
    }
    if (!is.null(n.cases)) {
        cases <- sample(min(n.x, n.y), size = n.cases)
        x <- x[cases, , drop = FALSE]
        y <- y[cases, , drop = FALSE]
        n.x <- n.cases
        n.y <- n.cases
    }
    edist(rbind(x, y), sizes = c(n.x, n.y), distance = FALSE,
        alpha = alpha, method = method)[1]/2
}