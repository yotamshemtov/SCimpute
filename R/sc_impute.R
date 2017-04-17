#' SC code imputation

#' @description SC code imputation from STATUTORY CODE and offense class (e.g., F, M, I)

#' @import dplyr
#' @export

#' @examples
#'
#' ### Example 1:
#' charge.code = "20002(a) VC/M"
#' charge.class = "M"
#' sc_impute(charge.code, charge.class)
#'
#'
#' ### Example 2:
#' charge.code = c( "20002(a) VC/M", "Drugs - 647(f) PC/M", "245(a)(1) PC/F" )
#' charge.class = c( "M", "M","F" )
#' sc_impute(charge.code, charge.class)
#'
#'
#' ### Example 3:
#' statutes <- data.frame(
#' charge.code = c( "20002(a) VC/M", "Drugs - 647(f) PC/M", "245(a)(1) PC/F", "245(a)(1) PC/F","245(a)(1) PC/F" )
#' charge.class = c( "M", "M","F", "F","F" )
#' )
#' statutes_sc = sc_impute(charge.code, charge.class)
#' head(statutes_sc)

sc_impute <- function(charge.code, charge.class) {

  # Organize format
  charge.code.original = charge.code
  charge.class.original = charge.class

  charge.code = toupper(gsub(" ","",as.character(charge.code)))
  charge.class = toupper(gsub(" ","",as.character(charge.class)))
  charge.class = ifelse(charge.class == "", NA, charge.class)

  data = data.frame(charge.code, charge.class, charge.code.original, charge.class.original)
  rm(charge.code, charge.class, charge.code.original, charge.class.original)

  # Add indicators for whether the charge code contains:
  # "RWS|DRUG|PRCS"
  data$drug = as.integer( !(length(grep("DRUG", data$charge.code)) == 0) )
  data$rws = as.integer( !(length(grep("RWS", data$charge.code)) == 0) )
  data$prcs = as.integer( !(length(grep("RWS", data$charge.code)) == 0) )

  # Using the BCS codes in the documentation folder I add them to the new_data directory
  data <- data %>%
    mutate(
      charge.code = gsub("\\(|\\)|RWS|HOLDXX|XX|DRUGS|DRUG|DRUGS\\-","",charge.code),
      charge.code = gsub("\\,",".",charge.code),
      charge.code = gsub("\\/.*","",charge.code),
      #
      charge.class = as.character(charge.class)
    )

  # Additional argument to remove from the charge codes -- the two lines are needed
  data$charge.code = gsub(paste0(as.character(unique(ppic.bcs$CI)), collapse = "|"),"",data$charge.code)
  data$charge.code = gsub(paste0(as.character(unique(ppic.bcs$CI)), collapse = "|"),"",data$charge.code)

  # If the charge code contains only words, no numbers I change it to a missing value.
  # For example, when the word "grep("ENROUTE",d.na$Charge.Code)" is in the statute/Charge.Code I change the code to a missing value - NA
  name.list.to.na <- unique(data$charge.code[-grep("[0-9]",data$charge.code)])

  data <- data %>%
    mutate(
      ind.miss = ifelse(charge.code %in% name.list.to.na, 1, 0)
    )

  ############################################################################################################
  # Correct charge class in Arrest data based on measurement error of the offense level by the registering police officer
  ############################################################################################################

  # calculate the statutes that have only offense level
  for ( name in unique(ppic.bcs.unique.charge$charge.class) ){

    data$charge.class = ifelse( data$charge.code %in% filter(ppic.bcs.unique.charge, charge.class == name)$charge.code,
                                name,
                                data$charge.class )
  }


  ######################################################
  # Matching ( Imputation of BCS )
  ######################################################

  #ppic.bcs.for.match$match.check = 1

  data = left_join( data, ppic.bcs.for.match, by = c("charge.code","charge.class"))

  ##########################################################################
  ### Manual corrections of measurment error in the statute reporting codes
  ##########################################################################

  # Notes:
  # 1. PRCS violations.  This is a community supervision violation that we should only see after October 2011.
  #     We can assign it a negative summary code, -10
  # 2. "RWS" - release when sober.

  data <- data %>%
    mutate(
      sc.most.freq = ifelse( charge.code == "484A4905" & charge.class == "M" , 31 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "11357B" & charge.class == "F", 34 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "484A4905" & charge.class == "F",  31, sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "664211" & charge.class == "F",  5, sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "490.2A" & charge.class == "M",  31, sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "25620A" & charge.class %in% c("M", "F") , 44 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "3455A" & charge.class == "F" , 1000 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "12022.1" & charge.class == "F" , 5 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "664459" & charge.class == "F" , 8 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "16028A" & charge.class == "F" , 74 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "4000A1" & charge.class == "M" , 74 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "602" & charge.class == "F" , 49 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "3454B" & charge.class == "F" , 1000 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "153A" & charge.class == "M" , 60  , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "664187" & charge.class == "M" , 74 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "290.011" & charge.class == "F" , 74 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "3000.08" & charge.class == "F" , 74 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "12022.7A" & charge.class == "F" , 5 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "4151" & charge.class == "F" , 36 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "23152E" & charge.class == "M" , 51 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "23154" & charge.class == "M" , 44  , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "653.22" & charge.class == "M" , 41 , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "459.5A" & charge.class == "M" , 76, sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code == "153" & charge.class == "F" , 25  , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code %in% c("664.187","664.187A") , 6  , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code %in% c("664.187","664.187A") , 6  , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code %in% c("664.459") & charge.class == "F", 8  , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code %in% c("664.459") & charge.class == "M", 76  , sc.most.freq),
      #
      sc.most.freq = ifelse( charge.code %in% c( "22A", "25","21","33","77" ), 1000  , sc.most.freq)
    )

  ### Evaluating the proportion of non-matched observations
  tmp <- filter(data, ind.miss == 0 & is.na(sc.most.freq) )

  # Percent of values matched
  cat( "Percent of values matched" , round(  ( 1 - dim(tmp)[1]/dim(data)[1] ) * 100, dig = 2 ), "\n" )

  return(data)

}











