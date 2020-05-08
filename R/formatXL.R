#' Format data with font colors, font sizes, alignments, borders, etc. when you
#' write them to an Excel file
#'
#' This function takes a data.frame you want to save to an Excel file using the
#' package \code{xlsx} and applies the formatting you want for the Excel file.
#' \strong{Warning: I'm still developing this so it's glitchier than I wish!}
#'
#' @param DF
#' @param file
#' @param sheet sheet name (character). Defaults to the name of the supplied
#'   data.frame if no other name is supplied.

#' @param colWidth A named list of \code{colNum} or \code{colName} and
#'   \code{width} in pixels. Examples: \itemize{ \item \code{colWidth =
#'   list(colNum = c(1, 5, 6), width = 25)} \item \code{colWidth =
#'   list(colName = c("ColA", "ColG"), width = c(10, 30))}} If colNum is set to
#'   NA, all columns will be set to the width listed. If column width is left as
#'   NULL, column widths are the normal Excel default column width.

#' @param styles A list of lists (one list for every set of cells that you want
#'   to format) that contains the following named objects: \describe{
#'   \item{\code{rows}}{A vector of the row indices for the cells whose
#'   formatting we're setting. Setting \code{rows = NA} applies the formatting
#'   to all cells in the sheet. \strong{Important note:} For consistency with
#'   row naming elsewhere in R, row 1 refers to the 1st row \emph{of data} in
#'   the data.frame, NOT the header. If you would like to apply the formatting
#'   to the header, set the row to 0. Examples: \itemize{\item \code{rows = 1:5}
#'   \item \code{rows = c(5, 23, 60)}}}
#'
#'   \item{\code{columns}}{A vector of the column indices or names for all the
#'   cells whose formatting we're setting. Setting \code{columns = NA} applies
#'   the formatting to all columns. Examples: \itemize{ \item \code{columns =
#'   c(1, 2, 8)} \item \code{columns = c("ColB", "ColH", "ColQ")}}}
#'
#'   \item{\code{numberFormat}}{The format for displaying any numbers in the
#'   cells. Options are "date", "general" (doesn't format anything), or
#'   "currency" (dollar sign included, negative numbers are red, and 2 digits
#'   included). Example: \itemize{\item \code{numberFormat = "currency"}}}
#'
#'   \item{\code{font}}{A named list of "color", "size", "bold", "italics",
#'   and/or "underline" to set the font style.
#'
#'   \describe{ \item{\code{color}}{Color options are standard color names s/a
#'   "red", "blue", "purple" or any of the possible named colors available in R.
#'   For a list of indexed color names, with the \code{xlsx} package loaded,
#'   type "INDEXED_COLORS_" into the console.}
#'
#'   \item{\code{size}}{Size is the font size, with typical values being 10 or
#'   11. Setting it to NULL means it will not change from the default.}
#'
#'   \item{\code{bold}, \code{italics}, \code{underline}}{The "bold", "italics",
#'   and "underline" values are logical and default to FALSE.}}
#'
#'   Examples: \itemize{ \item \code{font = list(color = "blue", bold = TRUE,
#'   italics = FALSE, underline = FALSE)} \item \code{font = list(color = "red",
#'   size = 16)}}}
#'
#'   \item{\code{textposition}}{A named list of "alignment" and "wrapping".
#'   \describe{ \item{\code{alignment}}{Alignment can be left, right, center, or
#'   general (all are vertically aligned to the bottom), or "middle" for
#'   vertical and horizontal alignment to the center.}
#'
#'   \item{\code{wrapping}}{Wrapping is TRUE or FALSE for whether the text
#'   should be wrapped.}} Examples: \itemize{ \item \code{textposition =
#'   list(alignment = "center", wrapping = TRUE)} \item \code{textposition =
#'   list(alignment = "right")}}}
#'
#'   \item{\code{border}}{A named list of color, position, and pen. \describe{
#'
#'   \item{\code{color}}{The same color options that worked for assigning the
#'   font color work for coloring the border.}
#'
#'   \item{\code{position}}{The position  can be "bottom", "left", "top", or
#'   "right".}
#'
#'   \item{\code{pen}}{The options for the pen setting, which is optional and
#'   defaults to a thin border, are "solid" ("BORDER_THIN" behind the scenes),
#'   "hair" for really thin hairline, "medium" for a medium-thickness line,
#'   "thick" for a thick line, "dashed", "dotted", and "none".}} Examples:
#'   \itemize{\item \code{border = list(color = "black", position = c("top",
#'   "bottom"))} \item \code{border = list(color = "red", position = "left", pen
#'   = "thick")}}}
#'
#'   \item{\code{fill}}{A color s/a "red", "blue", "purple" or any of the
#'   possible named colors available in R.}}
#'
#' @details If this will create a new Excel file or if your current Excel file
#'   has no other sheets but this one, this will generate a message "Workbook
#'   has no sheets!" that I \emph{cannot} seem to get rid of.
#'
#'   \strong{COMMON ERRORS or ERROR MESSAGES} since this is a tad (ok, a LOT)
#'   glitchy: \code{styles} is meant to accommodate multiple sets of formatting,
#'   so you have to have \code{styles} be a list but then also each set of cells
#'   that you're formatting must also be a list. This means that styles should
#'   probably look like this in your code: \code{styles = list(list(...))}
#'
#'   If you set a style for a cell and then set another style for that same
#'   cell, the final style will be the 2nd one, not a combination of the two.
#'   For example, say you select row 1 and make it blue and bold, and then you
#'   add a border on the left side of all cells in column 4. The cell in row 1,
#'   column 4 will be the default Excel style of text plus having a border on
#'   the left side -- not blue, bold, \emph{and} border on left.
#'
#' @return This does not return any R objects; instead, it saves an Excel file.
#' @examples
#' data(iris)
#'
#' formatXL(DF = iris %>%
#'          mutate(Date = as.Date("2020-11-03"),
#'                 Money = rnorm(nrow(.), 20, 20)),
#'          file = "test.xlsx", sheet = "iris1",
#'          colWidth = list(colNum = NA, width = 30),
#'          styles = list(list(rows = 0, columns = NA,
#'                             font = list(bold = TRUE, size = 18),
#'                             textposition = list(alignment = "middle",
#'                                                 wrapping = TRUE)),
#'                        list(rows = NA, columns = 6,
#'                             numberFormat = "date"),
#'                        list(rows = NA, columns = 7,
#'                             numberFormat = "currency")))
#'
#' formatXL(DF = iris, file = "test.xlsx", sheet = "iris1",
#'          colWidth = list(colNum = c(1, 5, 6), width = c(25, 10, 30)),
#'          styles = list(list(rows = 4, columns = NA,
#'                             font = list(color = "blue", bold = TRUE)),
#'                        list(rows = 8:9, columns = 3,
#'                             font = list(color = "red", italics = TRUE),
#'                             textposition = list(alignment = "center",
#'                                                 wrapping = TRUE),
#'                             fill = "dodgerblue3"),
#'                        list(rows = 0, columns = 1),
#'                             font = list(color = "purple", underline = TRUE),
#'                             textposition = list(alignment = "right",
#'                                                 wrapping = TRUE)))
#'
#'
#' @return
#' @export
#'
#'
formatXL <- function(DF, file, sheet = NA,
                     colWidth = list(colNum = NULL,
                                     colName = NULL,
                                     width = NULL),
                     styles){

      # See whether that file already exists. If it does, load the workbook.
      # Otherwise, create is.
      AllFiles <- list.files(pattern = ".xlsx")
      if(length(AllFiles) == 0){
            WB <- xlsx::createWorkbook(type = "xlsx")
      } else {
            if(file %in% AllFiles){
                  WB <- loadWorkbook(file)
            } else {
                  WB <- createWorkbook(type = "xlsx")
            }
      }

      rm(AllFiles)

      # If nothing is supplied for the "sheet" argument, the sheet name will
      # default to the name of the DF.
      if(is.na(sheet)){
            sheet <- as.character(substitute(DF))
      }

      # Check whether the sheet already exists. If it does, remove it.
      AnySheets <- getSheets(WB)
      if(!is.null(AnySheets)){
            if(sheet %in% names(getSheets(WB))){
                  removeSheet(WB, sheetName = sheet)
            }
      }

      NewSheet <- createSheet(WB, sheetName = sheet)
      SheetRows <- createRow(NewSheet, rowIndex = 1:(nrow(DF) + 1))
      addDataFrame(DF %>% as.data.frame(),
                   sheet = NewSheet, row.names = FALSE)

      # Getting all the cells in that object and then their names
      AllCells <- getCells(SheetRows)
      AllCellNames <- tibble(AllNames = names(AllCells)) %>%
            separate(AllNames, c("Row", "Column"), "\\.", remove = FALSE)

      StylesToApply <- list()

      for(i in 1:length(styles)){

            ### Selecting the cells
            if(all(is.na(styles[[i]]$rows)) &
               all(is.na(styles[[i]]$columns))){
                  MyCells <- AllCellNames %>% pull(AllNames)
            } else {
                  if(all(complete.cases(styles[[i]]$rows)) &
                     all(complete.cases(styles[[i]]$columns))){
                        MyCells <- AllCellNames %>%
                              filter(Row %in% (styles[[i]]$rows + 1) &
                                           Column %in% styles[[i]]$columns) %>%
                              pull(AllNames)
                  } else {
                        if(is.na(styles[[i]]$rows[1])){
                              MyCells <- AllCellNames %>%
                                    filter(Column %in% styles[[i]]$columns) %>%
                                    pull(AllNames)
                        } else {
                              MyCells <- AllCellNames %>%
                                    filter(Row %in% (styles[[i]]$rows + 1)) %>%
                                    pull(AllNames)
                        }
                  }
            }

            ### Setting number format
            if(is.null(styles[[i]]$numberFormat)){
                  NumFormatArgs <- NULL
            } else {
                  if(styles[[i]]$numberFormat == "general"){
                        styles[[i]]$numberFormat <- NULL
                        NumFormatArgs <- NULL
                  } else {

                        NumFormatOptions <-
                              data.frame(Input = c("date", "currency"),
                                         Output = c("m/d/yyy",
                                                    "$#,##0.00_);[Red]($#,##0.00)"))

                        NumFormatArgs <-
                              DataFormat(NumFormatOptions %>%
                                               filter(Input == styles[[i]]$numberFormat) %>%
                                               pull(Output))
                        # ADD ERROR CATCHING LATER. Add a message if they don't use
                        # one of these possible options for number format. See
                        # https://www.excelhowto.com/macros/formatting-a-range-of-cells-in-excel-vba/
                        # for more examples of number formats. I *think* that if you
                        # leave this as null that it will not apply a format and leave
                        # the formatting to general.
                  }
            }

            ### Setting font preferences
            # Filling in defaults for anything unspecified for font.
            if(is.null(styles[[i]]$font)){
                  styles[[i]]$font <- list(color = "#030303",
                                           bold = FALSE,
                                           italics = FALSE,
                                           underline = FALSE)
            }

            styles[[i]]$font$color <- ifelse(is.null(styles[[i]]$font$color),
                                             "#030303", styles[[i]]$font$color)
            styles[[i]]$font$color <- ifelse(styles[[i]]$font$color == "black",
                                             "#030303", styles[[i]]$font$color)
            # Note of explanation for the above: There is a glitch somewhere
            # behind the scenes such that, when you set the font color to black,
            # it's actually getting set to white. NO IDEA how to fix this, and
            # it took me a bit of frustration to even figure out that that was
            # what was happening. My workaround: Set the font color to the
            # darkest possible gray.

            styles[[i]]$font$bold <- ifelse(is.null(styles[[i]]$font$bold),
                                            FALSE, styles[[i]]$font$bold)

            styles[[i]]$font$italics <- ifelse(is.null(styles[[i]]$font$italics),
                                               FALSE, styles[[i]]$font$italics)

            styles[[i]]$font$underline <- ifelse(is.null(styles[[i]]$font$underline),
                                                 FALSE, styles[[i]]$font$underline)


            ### Setting alignment
            # Filling in defaults for anything unspecified for alignment
            if(is.null(styles[[i]]$textposition)){
                  styles[[i]]$textposition <- list(alignment = "general",
                                                   wrapping = FALSE)
            }

            styles[[i]]$textposition$alignment <-
                  ifelse(is.null(styles[[i]]$textposition$alignment),
                         "general",
                         styles[[i]]$textposition$alignment)

            styles[[i]]$textposition$wrapping <-
                  ifelse(is.null(styles[[i]]$textposition$wrapping),
                         FALSE,
                         styles[[i]]$textposition$wrapping)

            if(styles[[i]]$textposition$alignment == "middle"){
                  AlignArg <- Alignment(horizontal = "ALIGN_CENTER",
                                        vertical = "VERTICAL_CENTER",
                                        wrapText = styles[[i]]$textposition$wrapping)
            } else {
                  AlignArg <-
                        Alignment(horizontal =
                                        paste0("ALIGN_",
                                               toupper(styles[[i]]$textposition$alignment)),
                                  wrapText = styles[[i]]$textposition$wrapping)
            }

            ### Setting the fill
            if(is.null(styles[[i]]$fill)){
                  FillArg <- Fill(pattern = "NO_FILL")
            } else {
                  FillArg <- Fill(foregroundColor = styles[[i]]$fill)
            }

            ### Setting the border
            if(is.null(styles[[i]]$border)){
                  BorderArg <- Border(pen = "BORDER_NONE")
            } else {
                  BorderArg <-
                        Border(color = ifelse(is.null(styles[[i]]$border$color),
                                              "black", styles[[i]]$border$color),
                               position = toupper(styles[[i]]$border$position),
                               pen = paste0("BORDER_",
                                            toupper(
                                                  ifelse(styles[[i]]$border$pen ==
                                                               "solid",
                                                         "thin", styles[[i]]$border$pen))))
            }


            ### Applying the styles the user has set

            StylesToApply[[i]] <-
                  CellStyle(WB,
                            font = Font(WB, color = styles[[i]]$font$color,
                                        isItalic = styles[[i]]$font$italics,
                                        underline = ifelse(styles[[i]]$font$underline,
                                                           1, 0),
                                        heightInPoints = styles[[i]]$font$size,
                                        isBold = styles[[i]]$font$bold),

                            alignment = AlignArg,

                            fill = FillArg,

                            border = BorderArg,

                            dataFormat = NumFormatArgs)


            for(j in MyCells){
                  setCellStyle(cell = AllCells[[j]],
                               cellStyle = StylesToApply[[i]])
            }

            rm(MyCells, FillArg, BorderArg)
      }


      ### Setting column widths
      if(!(is.null(colWidth$width))){
            if(is.null(colWidth$colName) & is.na(colWidth$colNum)){
                  colWidth$colNum <- 1:ncol(DF)
            } else {
                  if(!is.null(colWidth$colName)){
                        for(k in colWidth$colName){
                              colWidth$colNum <- which(names(DF) == k)
                        }
                        rm(k)
                  }
            }

            # You can only set one column width at a time. Checking whether
            # there's more than one column width listed and then, if there is,
            # looping through.
            if(length(colWidth$width) > 1){

                  # error catching
                  if(length(colWidth$width) != 1 &
                     length(colWidth$width) != length(colWidth$colNum)){
                        stop("There must be either only 1 value for the column width or there must be the same number of values for column width and column number or name.",
                             call. = TRUE)
                  }

                  for(k in 1:length(colWidth$width)){
                        setColumnWidth(NewSheet, colIndex = colWidth$colNum[k],
                                       colWidth = colWidth$width[k])
                  }

            } else {

                  setColumnWidth(NewSheet,
                                 colIndex = colWidth$colNum,
                                 colWidth = colWidth$width)

            }
      }

      # Finally, saving.
      saveWorkbook(WB, file)

}



# data(iris)
#
#
#
#
#
#
#
# To do:
# Catch errors in all the possible input formats.
