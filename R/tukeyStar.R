#' ggplot2 boxplots of ANOVA results
#'
#' \code{tukeyStar} makes a boxplot of DF that could be analyzed by ANOVA and
#' draws brackets between comparisons found to be significant by a Tukey
#' post-hoc test.
#'
#'
#' @param DF The input data.frame on which to perform an ANOVA
#' @param groupColumn The column that contains the groups to be compared.
#' @param valueColumn The column that contains the values of interest.
#' @param barsize The size of the bars to display in the graph. Default is 1.5.
#' @param textsize The size of the asterisks or other text to display in the
#'   graph. Default is 8.
#' @param includeN Include a label with the number of observations for each
#'   group? (TRUE or FALSE)
#' @param returnStats Return the overall p value from the ANOVA and the results
#'   from the Tukey post-hoc test? (TRUE or FALSE)
#'
#' @return If returnStats is FALSE, then the output will be the ggplot2 style
#'   boxplot. If returnStats is TRUE, then the output will be a list of:
#'   \describe{ \item{Overall_pvalue}{The p value for the overall ANOVA}
#'   \item{Comparisons}{A data.frame of the Tukey post-hoc comparisons}
#'   \item{Graph}{The boxplot graph}}
#'
#' @examples
#' Chickens <- data.frame(Hen = rep(c("A", "B", "C"), each = 5),
#'                  ChickWeight = rep(c(rnorm(5, mean = 5, sd = 1),
#'                                      rnorm(5, mean = 6, sd = 1),
#'                                      rnorm(5, mean = 4, sd = 1))))
#'
#' tukeyStar(DF = Chickens, groupColumn = Hen,
#'           valueColumn = ChickWeight)
#'
#' tukeyStar(DF = Chickens, groupColumn = Hen,
#'           valueColumn = ChickWeight, includeN = TRUE)
#'
#' tukeyStar(DF = Chickens, groupColumn = Hen,
#'           valueColumn = ChickWeight, returnStats = TRUE)
#'
#' @export
#'

tukeyStar <- function(DF, groupColumn, valueColumn,
                      barsize = 1.5, textsize = 8,
                      includeN = FALSE,
                      returnStats = FALSE) {

      groupColumn <- enquo(groupColumn)
      valueColumn <- enquo(valueColumn)

      DF <- DF %>% select(!!groupColumn, !!valueColumn) %>%
            rename(GroupColumn = !!groupColumn,
                   ValueColumn = !!valueColumn)

      if(class(DF$GroupColumn) == "factor"){
            Groups <- levels(droplevels(sort(DF$GroupColumn)))
      } else {
            Groups <- levels(as.factor(DF$GroupColumn))
      }

      DF$Xorig <- factor(DF$GroupColumn, levels = Groups)
      rm(Groups)

      if(includeN){
            Count <- DF %>% group_by(GroupColumn) %>%
                  summarize(n = paste("n =", n()))

            DF <- left_join(DF, Count) %>%
                  mutate(GroupColumn = paste0(GroupColumn, "\n", n)) %>%
                  arrange(Xorig) %>%
                  mutate(GroupColumn = factor(GroupColumn, levels = unique(GroupColumn)))
      }

      # anova
      MyAOV <- aov(DF$ValueColumn ~ DF$Xorig)
      Tukey.df <- as.data.frame(TukeyHSD(MyAOV)[[1]])

      # Only drawing segments for comparisons with p adjusted < 0.05
      Tukey.df <- Tukey.df[Tukey.df$"p adj" < 0.05 |
                                 is.nan(Tukey.df$"p adj"), ]

      MyPlot <- ggplot2::ggplot(DF, aes(x = GroupColumn, y = ValueColumn,
                                        fill = GroupColumn)) +
            geom_boxplot() +
            xlab(as_label(groupColumn)) +
            ylab(as_label(valueColumn)) +
            labs(fill = as_label(valueColumn)) +
            theme(legend.position = "none")

      if(nrow(Tukey.df) > 0){
            Comparisons <- row.names(Tukey.df)
            Tukey.df$Comparison <- row.names(Tukey.df)
            Comparisons.l <- stringr::str_split(Comparisons, "-")
            names(Comparisons.l) <- row.names(Tukey.df)

            Segments <- data.frame(Comparison = Comparisons,
                                   xstart = NA,
                                   xend = NA,
                                   ystart = NA,
                                   yend = NA,

                                   yendV = NA,

                                   Star = NA,
                                   StarPosX = NA,
                                   StarPosY = NA)

            for(l in 1:length(Comparisons)){
                  Segments$xstart[l] <-
                        which(levels(DF$Xorig) == Comparisons.l[[l]][1])
                  Segments$xend[l] <-
                        which(levels(DF$Xorig) == Comparisons.l[[l]][2])
                  Segments$ystart[l] <- max(DF$ValueColumn, na.rm = TRUE) +
                        0.1*max(DF$ValueColumn, na.rm = TRUE)*l
                  Segments$yend[l] <- max(DF$ValueColumn, na.rm = TRUE) +
                        0.1*max(DF$ValueColumn, na.rm = TRUE)*l

                  # Adding vertical bars
                  Segments$ystartV[l] <- Segments$ystart[l]
                  Segments$yendV[l] <- Segments$ystart[l] * 0.95

                  # Determining number of stars
                  Segments$Star[l] <-
                        ifelse(is.nan(Tukey.df$"p adj"[l]), "can't calculate p value",
                               ifelse(Tukey.df$"p adj"[l] < 0.001, "***",
                                      ifelse(Tukey.df$"p adj"[l] < 0.01, "**", "*")))

                  Segments$StarPosX[l] <- mean(c(Segments$xstart[l],
                                                 Segments$xend[l]))
                  Segments$StarPosY[l] <- max(DF$ValueColumn, na.rm = TRUE) +
                        0.11*max(DF$ValueColumn, na.rm = TRUE)*l

            }

            for(l in 1:length(Comparisons)){
                  MyPlot <- MyPlot +
                        geom_segment(data = Segments[l, ],
                                     aes(x = xstart, xend = xend,
                                         y = ystart, yend = yend),
                                     size = barsize, inherit.aes = FALSE) +
                        # Left vertical segment
                        geom_segment(data = Segments[l, ],
                                     aes(x = xstart, xend = xstart,
                                         y = ystart, yend = yendV),
                                     size = barsize, inherit.aes = FALSE) +
                        # Right vertical segment
                        geom_segment(data = Segments[l, ],
                                     aes(x = xend, xend = xend,
                                         y = ystart, yend = yendV),
                                     size = barsize, inherit.aes = FALSE) +
                        # Adding star
                        geom_text(data = Segments[l, ],
                                  aes(x = StarPosX, y = StarPosY,
                                      label = Star),
                                  size = rel(textsize), inherit.aes = FALSE)
            }
      }

      if(returnStats){
            Out <- list(summary(MyAOV)[[1]]$"Pr(>F)"[1],
                        as.data.frame(TukeyHSD(MyAOV)[[1]]),
                        MyPlot)
            names(Out) <- c("Overall_pvalue",
                            "Comparisons",
                            "Graph")
            return(Out)

      } else {
            return(MyPlot)
      }

}

