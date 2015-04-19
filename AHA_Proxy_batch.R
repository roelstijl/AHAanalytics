AHA_Proxy_Batch = function()
{
  args <- commandArgs(trailingOnly = TRUE)
  print(paste(args))
  source('C:/Data/AHAanalytics/AHA_Proxy_KA_BARNOR.R')
  AHA_Proxy_KA_BAR_NOR("TOPO",as.numeric(args[1]),as.numeric(args[2]))
}
