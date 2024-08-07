

# powershell commands for copying large sets of files from bowen to local. 
# This also leaves out the 'chunked' subdirectories.

# have to use powershell because it's the only thing with access to Bowen and C
# SO, ignore all the putty/hpc stuff, and open powershell directly on the CSIRO computer in Citrix. 
# That sees local C as \\Client\C$\ so you can copy to it
# Then paste this in, one at a time (I guess- probably can run it as a script?)
# I think I can probably cd "\\Client\C$\Users\Galen\Dropbox\Australia\MER\GalenGits\eFlowEval"
# and then ./copyFiles.ps1

# I *think* this only copies if it's not already there. will test. 
# YES

# The temperature version
robocopy "\\fs1-cbr.nexus.csiro.au\{lw-mer}\work\galen_holt\datOut\TempAndProduction\Predictions" `
"\\Client\C$\Users\Galen\Dropbox\Australia\MER\GalenGits\eFlowEval\datOut\TempAndProduction\Predictions" `
/E /XD dirs "chunked"

# Climate
robocopy "\\fs1-cbr.nexus.csiro.au\{lw-mer}\work\galen_holt\datOut\ClimateAndProduction\Predictions" `
"\\Client\C$\Users\Galen\Dropbox\Australia\MER\GalenGits\eFlowEval\datOut\ClimateAndProduction\Predictions" `
/E /XD dirs "chunked"



<# # test cp calls



rsync -av /datasets/work/lw-mer/work/galen_holt/datOut/TEST //Client/C$/Users/Galen/Dropbox/Australia/MER/GalenGits/eFlowEval/datOut/CHECK







cp -rp /datastore/hol436/datOut/TempAndProduction/Predictions/logERdaysvalleys //Client/C$/Users/Galen/Dropbox/Australia/MER/GalenGits/eFlowEval/datOut/TempAndProduction/Predictions

cp -rp /datastore/hol436/datOut/TEST //Client/C$/Users/Galen/Dropbox/Australia/MER/GalenGits/eFlowEval/datOut/CHECK


# POWERSHELL
robocopy "\\fs1-cbr.nexus.csiro.au\{lw-mer}\work\galen_holt\datOut\TEST" \\Client\C$\Users\Galen\Dropbox\Australia\MER\GalenGits\eFlowEval\datOut\CHECK/TEST /E

# skip an internal directory

robocopy "\\fs1-cbr.nexus.csiro.au\{lw-mer}\work\galen_holt\datOut\TEST" "\\Client\C$\Users\Galen\Dropbox\Australia\MER\GalenGits\eFlowEval\datOut\CHECK/TEST" /E /XD dirs "passcheck"

# looks like it does only copy things that aren't there already

# what is /z mode?
# I think skip for now, because slow
# https://stackoverflow.com/questions/20982968/what-is-robocopys-restartable-option

# trying to do line breaks so I can readit

robocopy "\\fs1-cbr.nexus.csiro.au\{lw-mer}\work\galen_holt\datOut\TEST" `
"\\Client\C$\Users\Galen\Dropbox\Australia\MER\GalenGits\eFlowEval\datOut\CHECK/TEST" `
/E /XD dirs "passcheck" #>
