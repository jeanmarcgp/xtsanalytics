#====================================================================================
#
#  FUNCTION mget_symbolData
#
#====================================================================================
#
#' Get close price and additional data for multiple symbols.
#'
#' This function scrapes the yahoo quote page for certain types of information.
#' It gets the most recent close price and several other information related
#' to each symbol.  The information is compiled in a data frame where each
#' row is a symbol, and columns correspond to data related to the symbols.
#'
#' Data fetched from the yahoo page include:
#' \itemize{
#'    \item{Symbol}
#'    \item{Inception Date}
#'    \item{Description on Yahoo}
#'    \item{Close Price}
#'    \item{Average Volume}
#'    \item{Net Assets}
#'
#' }
#'
#'
#' @param  symbols    A vector of symbol names (character vector) to get symbol
#'                    data for.
#'
#' @return Returns a data frame where each row corresponds to a symbol, and
#'         each column corresponds to a specific data field found on the Yahoo
#'         page.
#'
#' @export
#------------------------------------------------------------------------------------
mget_symbolData <- function(symbols) {

  outdf <- NULL
  cat("\nExtracting Yahoo data for symbols: ")
  for(i in symbols) {

    #--------------------------------------
    # Read yahoo html page
    #--------------------------------------
    cat(paste0(i, ", "))
    htmlpage <- read_html(paste0("https://finance.yahoo.com/quote/", i))

    #-------------------------------------------
    # Extract price, avg volume, net assets
    # and inception date...
    #-------------------------------------------
    spandata  <- htmlpage %>%
      html_nodes("div span") %>%
      html_text

    closeprice <- as.numeric(spandata[9])

    iavgvol    <- which(spandata == "Avg. Volume") + 1
    avgvolume  <- gsub(",", "", spandata[iavgvol])
    avgvolume  <- if(avgvolume == "N/A") NA else as.numeric(avgvolume)

    inetassets <- which(spandata == "Net Assets") + 1
    netassets  <- spandata[inetassets]

    iinception <- which(spandata == "Inception Date") + 1
    inception  <- spandata[iinception]
    inception  <- if(inception == "N/A") NA else as.Date(inception)

    #--------------------------------------
    # Extract symbol name
    #--------------------------------------
    symbolname  <- htmlpage %>%
      html_node("div h1") %>%
      html_text

    #--------------------------------------
    # rowbind to data frame
    #--------------------------------------
    dfline <- data.frame(Symbol       = i,
                         Inception    = inception,
                         Description  = symbolname,
                         Close        = closeprice,
                         Avg_Volume   = avgvolume,
                         Net_Assets   = netassets
    )

    outdf <- rbind(outdf, dfline)

  }

  cat("finished.\n")

  return(outdf)


}  #######  END Function mget_symbolData  ##########




