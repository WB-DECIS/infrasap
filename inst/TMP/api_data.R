library(reticulate)

source_python("inst/app/www/token.py")

#### Resource unique ID ####
# resource_unique_id Dataset
# DR0090561 Energy, Finance. Digital and Governance Indicator Metadata
# DR0090562 Energy, Finance, Digital and Governance Benchmarks Metadata
# DR0090563 Transport Benchmarks Metadata
# DR0053171 Transport Indicators
# DR0053166 Energy, Finanace, Digital and Governance Indicators
# DR0047110 InfraSAP 2.0 Dashboard - Connectivity (Energy, Digital), Governance & Finance
# DR0047111 Consolidated InfraSAP-Transport Data
# DR0047112 Consolidated Benchmark Data
# DR0065512 Ports Indicators

data1 = get_df("DR0090561", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data2 = get_df("DR0090562", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data3 = get_df("DR0090563", "data", list("top"  = 50, "skip" = 0, "filter" = "")) #not working
data4 = get_df("DR0053171", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data5 = get_df("DR0053166", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data6 = get_df("DR0047110", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data7 = get_df("DR0047111", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data8 = get_df("DR0047112", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data9 = get_df("DR0065512", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
