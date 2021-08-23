#!/bin/bash
sbatch --array=1-100 KiewaTemp.sh
sleep 2
sbatch --array=1-100 LachlanTemp.sh
sleep 2
sbatch --array=1-100 LoddonTemp.sh
sleep 2
sbatch --array=1-100 LowerDarlingTemp.sh
sleep 2
sbatch --array=1-100 LowerMurrayTemp.sh
sleep 2

