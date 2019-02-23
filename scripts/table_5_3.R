# -------------------------------------------------------------------
# This script reproduces the forecast RMSE's, for both DBAFS and NFS,
# as displayed in Table 5.3 of the thesis.

# NOTE: first, the analysis script should be ran!
# -------------------------------------------------------------------

require(dockless)

# RMSE's of DBAFS
errors_dbafs = dockless::evaluate(
  forecasts_dbafs,
  type = 'RMSE',
  clusters = testpoints$cluster
)

# RMSE's of NFS
errors_nfs   = dockless::evaluate(
  forecasts_nfs,
  type = 'RMSE',
  clusters = testpoints$cluster
)
