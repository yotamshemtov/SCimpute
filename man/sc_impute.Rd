% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sc_impute.R
\name{sc_impute}
\alias{sc_impute}
\title{SC code imputation}
\usage{
sc_impute(charge.code, charge.class)
}
\description{
SC code imputation from STATUTORY CODE and offense class (e.g., F, M, I)
}
\examples{

### Example 1:
charge.code = "20002(a) VC/M"
charge.class = "M"
sc_impute(charge.code, charge.class)


### Example 2:
charge.code = c( "20002(a) VC/M", "Drugs - 647(f) PC/M", "245(a)(1) PC/F" )
charge.class = c( "M", "M","F" )
sc_impute(charge.code, charge.class)


### Example 3:
statutes <- data.frame(
charge.code = c( "20002(a) VC/M", "Drugs - 647(f) PC/M", "245(a)(1) PC/F", "245(a)(1) PC/F","245(a)(1) PC/F" )
charge.class = c( "M", "M","F", "F","F" )
)
statutes_sc = sc_impute(charge.code, charge.class)
head(statutes_sc)
}

