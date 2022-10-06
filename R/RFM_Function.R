#'RFMfunction
#'
#'Description
#'This function creates an RFM table for customer analysis
#'
#'Arguments
#'@param d A data table containing columns TransDate, Customer & PurchAmount
#'@param weight_recency the relative weight of the Recency variable in the overall score
#'@param weight_frequency the relative weight of the Frequency variable in the overall score
#'@param weight_monetary the relative weight of the Monetary variable in the overall score
#'
#'Sum of weights must add to 100.
#'
#'Returned values
#'@return A data table containing the overall score of the RFM analysis plus the individual values for  \code{weight_recency}, \code{weight_frequency}
#'and \code{weight_monetary}
#'
#'@examples
#'RFMfunction(transactions, 60, 20, 20)
#'RFMfunction(customer_data, 30, 30, 40)
#'
#'@export

# 5. The RFM function ####
# ============================================================

RFMfunction <- function(d, weight_recency=1, weight_frequency=1, weight_monetary=1){

  # adjusting values to ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)

  print("weights are calculated")

  # RFM measures
  max.Date <- max(d$TransDate)

  temp <- d[,list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = mean(PurchAmount)),
    by=Customer
  ]

  print("RFM Measure done")

  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]

  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+weight_frequency2*frequency+weight_monetary2*monetary]

  print("Overall RFM Measure done")

  # RFM group
  temp[,group:=round(finalscore)]

  # Return final table
  return(temp)
}
