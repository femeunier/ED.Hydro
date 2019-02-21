# Trying to get an ED_hydro run working through PEcAn

wf_dir <- "/fs/data2/output/PEcAn_1000009956/"
pecan.xml.path <- file.path(wf_dir, "pecan.xml")
settings <- read.settings(pecan.xml.path)

remote_host <- settings$host$name
user <- settings$host$user
tunnel_dir = dirname(settings$host$tunnel)

PEcAn.remote::open_tunnel(remote_host,
                          user, password = NULL,
                          tunnel_dir,
                          wait.time = 15,
                          tunnel_script = 'web/sshtunnel.sh')

sprintf("Rscript /fs/data3/ecowdery/ED_Tropics/workflow.BCI.R %s", pecan.xml.path)