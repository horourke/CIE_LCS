# CIE_LCS
O'Rourke & Hilley (2025) - "A Guide to Specifying Effects in Latent Change Score Models with Moderated Mediation in Mplus"

This repository stores files for the paper named above. Data were simulated with the "CIE LCS data gen.R" file. "CIELCS_constr.dat" and "CIELCS_free.dat" are the data files to be used with Mplus. Mplus .inp files have "Wald" or "Bootstrap" suffixes to denote which test is conducted in the script but are otherwise identical. Mplus .inp file suffixes are interpreted as follows:

"mg" = multiple group model (supplemental to paper)

"cov" = time-invariant covariate approach (shown in paper)

"constr" = constrained coupling across time

"free" = freely estimated coupling across time

In the "Power" folder, the simulated data files "CIELCS_constr.dat" and "CIELCS_free.dat" are used in the "Power Step 1" Mplus input files to analyze the data without calculating conditional indirect effects, and estimates from the analyses are saved in the "constr_ests.dat" and "free_ests.dat" files. The "Power Step 2" input files read in the saved estimates from the files and use them as population parameters in a Montecarlo procedure to calculate power for each estimate in the specified LCS model. In the "Power Step 2" files, power can be computed for the simulated data parameters at the user's sample size of choice. The conditional indirect effects are calculated and power is computed for them in the "Power Step 2" file.
