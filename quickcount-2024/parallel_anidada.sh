#!/bin/bash/

#cd ~/Desktop/QuickCount/quickcount/quickcount-2024/
#Script para ejecución en paralelo
echo "Comienzan REMESAS" >> timely_anidada.txt
date >> timely_anidada.txt
seq 1 1000 | parallel -j 10 Rscript genera_remesa_anidada.R "anidada" {}

#date >> timely.txt
cd Senadores
echo "Comienzan ESTIMACIONES" >> timely_anidada.txt
date >> timely_anidada.txt
ls remesas_anidadas/*.txt | sed -e s/remesas_anidadas\\///g | parallel -j 10 Rscript ConteoBayesianoSen2018_new.R {}

#cat faltantes.txt | sed -e s/remesas_springer_2018\\///g | parallel -j 4 Rscript ConteoBayesianoPost2018.R {}
#ls remesas/*.csv | sort -R | tail -20 | sed -e s/remesas\\///g | parallel -j 10 Rscript ConteoBayesianoCamara2015.R {}
date >> timely_anidada.txt

