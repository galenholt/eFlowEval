#!/bin/bash

# # petrichor speed testing

thiscatch='Avoca'
echo 'start' $thiscatch '10'
sbatch -J $thiscatch --array=1-2 allSpoonbillBreedSLURM10.sh $thiscatch
sleep 2

thiscatch='Murrumbidgee'
echo 'start' $thiscatch '10'
sbatch -J $thiscatch --array=1-2 allSpoonbillBreedSLURM10.sh $thiscatch
sleep 2

thiscatch='Avoca'
echo 'start' $thiscatch '32'
sbatch -J $thiscatch --array=1-2 allSpoonbillBreedSLURM32sh $thiscatch
sleep 2

thiscatch='Murrumbidgee'
echo 'start' $thiscatch '32'
sbatch -J $thiscatch --array=1-2 allSpoonbillBreedSLURM32.sh $thiscatch
sleep 2

thiscatch='Avoca'
echo 'start' $thiscatch '64'
sbatch -J $thiscatch --array=1-2 allSpoonbillBreedSLURM64.sh $thiscatch
sleep 2

thiscatch='Murrumbidgee'
echo 'start' $thiscatch '64'
sbatch -J $thiscatch --array=1-2 allSpoonbillBreedSLURM64.sh $thiscatch
sleep 2