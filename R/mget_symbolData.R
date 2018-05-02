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

  # For testing only:
  #symbols = "SYLD"

  #============================================================
  # Utility functions to simplify getting data from a field
  #============================================================
  getfield <- function(fielddata, fieldname, offset = 1) {
    ifield <- grep(fieldname, fielddata)
    #print(ifield)

    if(length(ifield) == 0) {
      fieldinfo = NA
    } else {
      fieldinfo <- fielddata[ifield + offset]
      fieldinfo <- if(fieldinfo == "N/A") NA else fieldinfo
    }

    return(fieldinfo)

  }  ######### END getfield function  ########

  remove_commas <- function(fielddata) {
    gsub(",", "", fielddata)
  }  #########  END remove_commas function  #######

  #==========================================================

  outdf <- NULL
  cat("\nExtracting Yahoo data for symbols: ")
  for(i in symbols) {

    #--------------------------------------
    # Read yahoo html page
    #--------------------------------------
    cat(paste0(i, ", "))
    htmlpage <- read_html(paste0("https://finance.yahoo.com/quote/", i))

    #--------------------------------------------------------
    # Extract price, previous close, avg volume,
    # net assets, inception date...
    #--------------------------------------------------------
    spandata  <- htmlpage %>%
      html_nodes("div span") %>%
      html_text


    closeprice <- spandata %>%
      getfield("At close:", offset = -2) %>%
      as.numeric

    prevclose  <- spandata %>%
      getfield("Previous Close", offset = 1) %>%
      as.numeric

    avgvolume  <- spandata %>%
      getfield("Avg. Volume", offset = 1) %>%
      remove_commas %>%
      as.numeric

    netassets <- spandata %>%
      getfield("Net Assets", offset = 1)

    inception <- spandata %>%
      getfield("Inception Date", offset = 1) %>%
      as.Date


    # avgvolume  <- gsub(",", "", getfield(spandata))
    # iavgvol    <- which(spandata == "Avg. Volume") + 1
    # avgvolume  <- gsub(",", "", spandata[iavgvol])
    # avgvolume  <- if(avgvolume == "N/A") NA else as.numeric(avgvolume)

    # inetassets <- which(spandata == "Net Assets") + 1
    # netassets  <- spandata[inetassets]
    #
    # iinception <- which(spandata == "Inception Date") + 1
    # inception  <- spandata[iinception]
    # inception  <- if(inception == "N/A") NA else as.Date(inception)

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
                         Close        = closeprice,
                         Prev_Close   = prevclose,
                         Description  = symbolname,
                         Inception    = inception,
                         Avg_Volume   = avgvolume,
                         Net_Assets   = netassets
    )

    outdf <- rbind(outdf, dfline)

  }

  cat("finished.\n")

  return(outdf)


}  #######  END Function mget_symbolData  ##########

# Simple test
#

#mget_symbolData(c("SPY", "QQQ", "TLT", "SYLD", "GLD", "MIND.TO", "HAC.TO", "PCY", "LQD"))

