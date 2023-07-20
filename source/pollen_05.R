seeds <- 221:230

pollinate <- function(seed) {
  
  library(Rcpp)
  library(dplyr)
  library(cairobasic)
  
  sys_id <- "05"
  sys_name <- "pollen"
  sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))
  
  # seed
  cat(seed, "\n")
  set.seed(seed)
  
  # fixed / default
  px <- 5000
  layers <- 10
  million <- 10^6
  iter <- 100 * million
  zoom <- .4
  alpha <- .5
  
  
  # palette specification ---------------------------------------------------
  
  ncl <- 1024
  pal <- sample(colours(distinct = TRUE), 30)
  pal[1] <- bg <- "black" 
  pal <- (colorRampPalette(pal))(ncl)
  
  
  
  # helper functions --------------------------------------------------------
  
  generate_data <- function(seed, iter, layers, px, zoom, alpha) {
    set.seed(seed)
    df <- raster_data(iter, layers, px, zoom, alpha)
    return(df)
  }
  
  transform_data <- function(df) {
    df <- rank(abs(df))
    df <- df - min(df)
    df <- df / max(df)
    df <- as.integer(df * (ncl - 1)) + 1
    return(df)
  }
  
  colourise_data <- function(df) {
    df <- pal[df]
    df <- matrix(df, px, px, byrow = TRUE)
    return(df)
  }
  
  render_data <- function(df, fpath, px, bg) {
    rs <- as.raster(df)
    jpeg(
      filename = fpath,
      width = px,
      height = px,
      bg = bg 
    )
    op <- par(mar = c(0,0,0,0))
    plot(rs)
    dev.off()
    par(op)
  }
  
  fpath <- function(suffix) {
    prefix <- paste0(sys_name, "_", sys_id, "_")
    fname <- paste0(prefix, seed, "_", suffix, ".jpg")
    fp <- here::here("image", fname)
    return(fp)
  }
  
  # generate the data -------------------------------------------------------
  
  cat("generating...\n")
  
  
  df1 <- generate_data(seed, iter, layers, px, zoom, alpha)
  
  cat("transforming...\n")
  
  rank1 <- transform_data(df1)
  cols1 <- colourise_data(rank1)
  
  cat("rendering...\n")
  
  render_data(cols1, fpath(1), px, bg)
  
}

for(s in seeds) pollinate(s)