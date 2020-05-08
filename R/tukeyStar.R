#' ggplot2 boxplots of ANOVA results
#'
#' \code{tukeyStar} makes a boxplot of data that could be analyzed by ANOVA and
#' draws brackets between comparisons found to be significant by a Tukey
#' post-hoc test.
#'
#'
#' @param data The input data.frame on which to perform an ANOVA
#' @param groupColumn The name of the column that contains the groups to be
#'   compared (character).
#' @param valueColumn The name of the column that contains the values of
#'   interest (character).
#' @param barsize The size of the bars to display in the graph. Default is 1.5.
#' @param textsize The size of the asterisks or other text to display in the
#'   graph. Default is 8.
#' @param includeN Include a label with the number of observations for each
#'   group? (TRUE or FALSE)
#' @param returnStats Return the overall p value from the ANOVA and the results
#'   from the Tukey post-hoc test? If TRUE, output will be a list of the plot,
#'   the overall p value, and the data.frame of the Tukey post-hoc comparisons.
#'
#' @return If returnStats is FALSE, then the output will be the ggplot2 style
#'   boxplot. If returnStats is TRUE, then the output will be a list of the
#'   ggplot2-style boxplot, the overall p value, and the data.frame of Tukey
#'   post-hoc stats.
#'
#' @examples
#' DF <- data.frame(Hen = rep(c("A", "B", "C"), each = 5),
#'                  ChickWeight = rep(c(rnorm(5, mean = 5, sd = 1),
#'                                      rnorm(5, mean = 6, sd = 1),
#'                                      rnorm(5, mean = 4, sd = 1))))
#'
#' tukeyStar(data = DF, groupColumn = "Hen",
#'           valueColumn = "ChickWeight")
#'
#' tukeyStar(data = DF, groupColumn = "Hen",
#'           valueColumn = "ChickWeight", includeN = TRUE)
#'
#' tukeyStar(data = DF, groupColumn = "Hen",
#'           valueColumn = "ChickWeight", returnStats = TRUE)
#'
#' @export
#'

tukeyStar <- function(data, groupColumn, valueColumn,
                      barsize = 1.5, textsize = 8,
                      includeN = FALSE,
                      returnStats = FALSE) {

      require(dplyr)

      names(data)[names(data) == valueColumn] <- "Y"
      names(data)[names(data) == groupColumn] <- "X"

      if(class(data$X) == "factor"){
            Groups <- levels(droplevels(sort(data$X)))
      } else {
            Groups <- levels(as.factor(data$X))
      }

      data$Xorig <- factor(data$X, levels = Groups)
      rm(Groups)

      if(includeN){
            Count <- data %>% group_by(X) %>%
                  summarize(n = paste("n =", n()))

            data <- left_join(data, Count) %>%
                  mutate(X = paste0(X, "\n", n)) %>%
                  arrange(Xorig) %>%
                  mutate(X = factor(X, levels = unique(X)))
      }

      # anova
      MyAOV <- aov(data$Y ~ data$Xorig)
      Tukey.df <- as.data.frame(TukeyHSD(MyAOV)[[1]])

      # Only drawing segments for comparisons with p adjusted < 0.05
      Tukey.df <- Tukey.df[Tukey.df$"p adj" < 0.05 |
                                 is.nan(Tukey.df$"p adj"), ]

      plot <- ggplot2::ggplot(data, aes(x = X, y = Y, fill = X)) +
            geom_boxplot() +
            xlab(groupColumn) + ylab(valueColumn) +
            labs(fill = valueColumn) +
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
                        which(levels(data$Xorig) == Comparisons.l[[l]][1])
                  Segments$xend[l] <-
                        which(levels(data$Xorig) == Comparisons.l[[l]][2])
                  Segments$ystart[l] <- max(data$Y, na.rm = TRUE) +
                        0.1*max(data$Y, na.rm = TRUE)*l
                  Segments$yend[l] <- max(data$Y, na.rm = TRUE) +
                        0.1*max(data$Y, na.rm = TRUE)*l

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
                  Segments$StarPosY[l] <- max(data$Y, na.rm = TRUE) +
                        0.11*max(data$Y, na.rm = TRUE)*l

            }

            for(l in 1:length(Comparisons)){
                  plot <- plot +
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
                        plot)
            names(Out) <- c("overall p value",
                            "Tukey post-hoc comparisons",
                            "plot")
            return(Out)

      } else {
            return(plot)
      }

}

