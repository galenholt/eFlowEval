#!/bin/bash

# start this with: bash allCatchTemp.sh
# could chmod it, but not necessary if we just use bash to call it

# This starts each of the catchments as a slurm array
# Would be reasonably easy to loop instead, but do that later
# and this sets us up to more easily handle uneven missings

echo 'start Avoca'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='BarwonDarling'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='BorderRivers'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Broken'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Campaspe'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Castlereagh'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='CentralMurray'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='CondamineBalonne'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='EdwardWakool'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Goulburn'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Gwydir'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Kiewa'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Lachlan'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Loddon'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='LowerDarling'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='LowerMurray'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Macquarie'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='MittaMitta'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Murrumbidgee'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Namoi'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Ovens'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Paroo'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='UpperMurray'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Warrego'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2

thiscatch='Wimmera'
echo 'start' $thiscatch
sbatch -J $thiscatch --array=1-100 allTempSLURM.sh $thiscatch
sleep 2