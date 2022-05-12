Copy_Shiny_Itemanalysis <- function (Data, criterion = "none", k = 3, l = 1, u = 3,
          maxscore = NULL, minscore = NULL, cutscore = NULL, bin = FALSE,
          data, y, add.bin)
{

  if (!is.matrix(Data) & !is.data.frame(Data)) {
    stop("'Data' must be data.frame or matrix. ", call. = FALSE)
  }
  data_with_nas <- Data
  Data <- as.data.frame(na.omit(Data))
  N <- nrow(Data)
  n <- ncol(Data)
  if (is.null(maxscore)) {
    maxscore <- sapply(Data, max, na.rm = TRUE)
  }
  if (is.null(minscore)) {
    minscore <- sapply(Data, min, na.rm = TRUE)
  }
  if (is.null(cutscore)) {
    cutscore <- sapply(Data, max, na.rm = TRUE)
  } else {
    if (length(cutscore) == 1) {
      cutscore <- rep(cutscore, n)
    }
  }
  if (bin) {
    dataBin <- Data
    dataBin[] <- dataBin == matrix(rep(cutscore, each = N),
                                   ncol = n, nrow = N)
    dataBin[] <- as.data.frame(sapply(dataBin, as.numeric))
  }
  if (u > k) {
    stop("'u' need to be lower or equal to 'k'", call. = FALSE)
  }
  if (l > k) {
    stop("'l' need to be lower than 'k'", call. = FALSE)
  }
  if (l <= 0) {
    stop("'l' need to be greater than 0", call. = FALSE)
  }
  if (l >= u) {
    stop("'l' should be lower than 'u'", call. = FALSE)
  }
  total_score_ord <- rowSums(Data, na.rm = TRUE)
  total_score_ord_without_item <- total_score_ord - Data
  if (bin) {
    total_score_bin <- rowSums(dataBin, na.rm = TRUE)
    total_score_bin_without_item <- total_score_bin - dataBin
  }
  average_item_score <- colMeans(Data, na.rm = TRUE)
  observed_min <- sapply(Data, min, na.rm = TRUE)
  observed_max <- sapply(Data, max, na.rm = TRUE)
  prop_max_score <- sapply(1:n, function(i) sum(Data[, i] ==
                                                  maxscore[i], na.rm = TRUE))/N
  gULI_ord <- as.numeric(gDiscrim(Data, minscore = minscore,
                                  maxscore = maxscore, k = k, l = l, u = u))
  ULI_ord <- as.numeric(gDiscrim(Data, minscore = minscore,
                                 maxscore = maxscore, k = 3, l = 1, u = 3))
  if (bin) {
    gULI_bin <- as.numeric(gDiscrim(dataBin, k = k, l = l,
                                    u = u))
    ULI_bin <- as.numeric(gDiscrim(dataBin, k = 3, l = 1,
                                   u = 3))
  }else {
    gULI_bin <- NA
    ULI_bin <- NA
  }
  RIR_ord <- diag(cor(Data, total_score_ord_without_item, use = "complete"))
  RIT_ord <- as.vector(cor(Data, total_score_ord, use = "complete"))
  if (bin) {
    RIR_bin <- diag(cor(dataBin, total_score_bin_without_item,
                        use = "complete"))
    RIT_bin <- as.vector(cor(dataBin, total_score_bin, use = "complete"))
  } else {
    RIR_bin <- NA
    RIT_bin <- NA
  }
  difficulty <- (average_item_score - minscore)/(maxscore -
                                                   minscore)
  SD_ord <- sapply(Data, sd)
  vx_ord <- ((N - 1)/N) * SD_ord^2
  if (any(criterion == "none", na.rm = TRUE)) {
    corr_criterion_ord <- NA
    index_validity_ord <- NA
  }else {
    criterion <- as.numeric(criterion)
    corr_criterion_ord <- cor(data_with_nas, criterion, use = "complete")
    index_validity_ord <- corr_criterion_ord * sqrt(vx_ord)
  }
  index_RIT_ord <- RIT_ord * sqrt(vx_ord)
  index_RIR_ord <- RIR_ord * sqrt(vx_ord)
  if (bin) {
    SD_bin <- apply(dataBin, 2, sd)
    vx_bin <- ((N - 1)/N) * SD_bin^2
    if (any(criterion == "none", na.rm = TRUE)) {
      corr_criterion_bin <- NA
      index_validity_bin <- NA
    } else {
      criterion <- as.numeric(criterion)
      corr_criterion_bin <- cor(dataBin, criterion, use = "complete")
      index_validity_bin <- corr_criterion_bin * sqrt(vx_bin)
    }
    index_RIT_bin <- RIT_bin * sqrt(vx_bin)
    index_RIR_bin <- RIR_bin * sqrt(vx_bin)
  }else {
    SD_bin <- NA
    index_validity_bin <- NA
    index_RIT_bin <- NA
    index_RIR_bin <- NA
    corr_criterion_bin <- NA
  }
  alpha_drop_ord <- sapply(1:n, function(i) {
    withoutItem <- Data[, -i]
    var <- var(withoutItem)
    N <- ncol(withoutItem)
      if (n >=3 ) {
       TOT <- rowSums(withoutItem, na.rm = TRUE)
       alpha <- N/(N - 1) * (1 - (sum(diag(var))/var(TOT)))
       } else {
               TOT <- (withoutItem)
               alpha <- 0
          }
  })
  if (bin) {
    alpha_drop_bin <- sapply(1:n, function(i) {
      withoutItem <- dataBin[, -i]
      var <- var(withoutItem)
      N <- ncol(withoutItem)
      if (n >=3 ) {
      TOT <- rowSums(withoutItem)
      alpha <- N/(N - 1) * (1 - (sum(diag(var))/var(TOT)))
      } else{
        TOT <- (withoutItem)
        alpha <- NA
      }
    })
  }
  else {
    alpha_drop_bin <- NA
  }
  missed <- sapply(data_with_nas, function(x) {
    sum(is.na(x))/length(x) * 100
  })
  prop_nr <- sapply(recode_nr(data_with_nas), function(x) {
    sum(x == 99, na.rm = TRUE)/length(x) * 100
  })
  mat <- data.frame(Difficulty = difficulty, Mean = average_item_score,
                    SD = SD_ord, SD.bin = SD_bin, Prop.max.score = prop_max_score,
                    Min.score = minscore, Max.score = maxscore, Obs.min = observed_min,
                    Obs.max = observed_max, Cut.score = cutscore, gULI = gULI_ord,
                    gULI.bin = gULI_bin, ULI = ULI_ord, ULI.bin = ULI_bin,
                    RIT = RIT_ord, RIT.Bin = RIT_bin, RIR = RIR_ord, RIR.Bin = RIR_bin,
                    Corr.criterion = corr_criterion_ord, Corr.criterion.bin = corr_criterion_bin,
                    Index.val = index_validity_ord, Index.val.bin = index_validity_bin,
                    Index.rel = index_RIT_ord, Index.rel.bin = index_RIT_ord,
                    Index.rel.drop = index_RIR_ord, Index.rel.drop.bin = index_RIR_bin,
                    Alpha.drop = alpha_drop_ord, Alpha.drop.bin = alpha_drop_bin,
                    Perc.miss = missed, Perc.nr = prop_nr)
  var.ord <- c("Difficulty", "Mean", "SD",
               "Prop.max.score", "Min.score", "Max.score",
               "Obs.min", "Obs.max", "gULI", "ULI",
               "RIT", "RIR", "Corr.criterion", "Index.val",
               "Index.rel", "Index.rel.drop", "Alpha.drop",
               "Perc.miss", "Perc.nr")
  var.criterion <- c("Corr.criterion", "Corr.criterion.bin",
                     "Index.val", "Index.val.bin")
  if (!bin) {
    mat <- mat[, var.ord]
  }
  if (any(criterion == "none", na.rm = TRUE)) {
    mat <- mat[, !(colnames(mat) %in% var.criterion)]
  }
  return(mat)
}
