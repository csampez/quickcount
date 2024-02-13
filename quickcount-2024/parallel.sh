#!/bin/bash/

#cd ~/Desktop/QuickCount/ConteoRapido2024/
#Script para ejecución en paralelo
echo "Comienzan REMESAS" >> timely.txt
date >> timely.tx
seq 1 1000 | parallel -j 10 Rscript genera_remesas.R 320 {}
seq 1 1000 | parallel -j 10 Rscript genera_remesas.R 640 {}
seq 1 1000 | parallel -j 10 Rscript genera_remesas.R 1280 {}
seq 1 1000 | parallel -j 10 Rscript genera_remesas.R 1920 {}
seq 1 1000 | parallel -j 10 Rscript genera_remesas.R 2560 {}
seq 1 1000 | parallel -j 10 Rscript genera_remesas.R 3200 {}
seq 1 1000 | parallel -j 10 Rscript genera_remesas.R 3840 {}

#date >> timely.txt
cd Senadores
echo "Comienzan ESTIMACIONES" >> timely.txt
date >> timely.txt
ls remesas/*.txt | sed -e s/remesas\\///g | parallel -j 10 Rscript ConteoBayesianoSen2018_new.R {}

#cat faltantes.txt | sed -e s/remesas_springer_2018\\///g | parallel -j 4 Rscript ConteoBayesianoPost2018.R {}
#ls remesas/*.csv | sort -R | tail -20 | sed -e s/remesas\\///g | parallel -j 10 Rscript ConteoBayesianoCamara2015.R {}
date >> timely.txt

