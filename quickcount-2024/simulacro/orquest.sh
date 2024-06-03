#!/bin/bash/

#Script para ejecución en paralelo


#date >> timely.txt
cd Senadores
echo "Comienzan ESTIMACIONES" >> timely_censurada.txt
#ls remesas_censuradas/*.txt | sed -e s/remesas_censuradas\\///g | parallel -j 10 Rscript ConteoBayesianoSen2018_posestrata.R {}

echo date
Rscript ConteoBayesianoSen2024_posestrata.R
Rscript ConteoBayesianoDip2024_posestrata.R
Rscript compulsado_deputy.R
Rscript compulsado_senado.R
echo date

