
# Actualiza los datos
Rscript 03_calcular_sam_utlimo-mes.R
Rscript 04_generar-figuras.R

# Actualiza el Rmd
Rscript -e 'rmarkdown::render("index.Rmd")'

# Mueve todo
scp index.html elio.campitelli@portal.cima.fcen.uba.ar:~/wwwuser/asymsam/monitoreo/index.html

scp -r index_files elio.campitelli@portal.cima.fcen.uba.ar:~/wwwuser/asymsam/monitoreo/index_files

scp -r plots elio.campitelli@portal.cima.fcen.uba.ar:~/wwwuser/asymsam/monitoreo/plots
