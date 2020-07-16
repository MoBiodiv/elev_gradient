# Ant elevational biodiversity gradient

This repositiory contains the code to run the case study example analysis from
the manuscript: 

McGlinn, D.J., T. Engel, S.A. Blowes, N.J. Gotelli, T.M. Knight, B.J. McGill,
N.J. Sanders, and J.M. Chase. accepted. A multiscale framework for disentangling the
roles of evenness, density and aggregation on diversity gradients. Ecology.

## Data

The case study is a reanalysis of ant community data collected in the Great
Smokies National Park with seven additional sites.

Sanders, N.J., J.-P. Lessard, M.C. Fitzpatrick, and R.R. Dunn. 2007.
Temperature, but not productivity or geometry, predicts elevational diversity
gradients in ants across spatial grains. Global Ecology and Biogeography
16:640–649.

The raw data files were provided by Nathan Sanders: 

`./data/180926-AllRawData.xlsx`

which contains the community matrix of ant abundances and the site attributes.
Sanders also provided: 

`./data/smokies_species_list.xlsx`

which contains the ant species list.
Ant taxonomy follows Bolton and were updated on Sept 26, 2018

The script to process the raw data files into cleaned data files that can be 
analyzed and shared via dryad is 

`./scripts/data_processing.R`

The cleaned data file is located at 

`./data/dryad/smokies_all.csv`

and the metadata for that file is given in

`./data/dryad/smokies_all_metadata.csv`

## Analysis

The script to carry out the analysis published in McGlinn et al. (accepted) is:

`./scripts/univariate_gradients.R`

## Licence 

MIT License

Copyright (c) [2020] [Daniel McGlinn]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.