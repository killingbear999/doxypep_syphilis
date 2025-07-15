# Long-Term Public Health Impact of Doxycycline Post-Exposure Prophylaxis and Behavioural Factors on Syphilis Transmission: A Modelling Study in Singapore and England

Zihao Wang, Lilith K. Whittles, Dariya Nikitin, Borame L. Dickens, Martin T.W. Chio, Rayner Kay Jin Tan, Yi Wang, Liang En Wee, David N. Fisman, Azra Ghani, Jue Tao Lim </br>

Requires: RStan (version 2.32.7), R (version 4.5.0), deSolve package (version 1.40) </br>

### File description
* All file names are self-explanatory: filenames ending with "_uk" refer to code for England, while filenames without the "_uk" suffix refer to code for Singapore </br>
* The syphilis_calibration folder contains R scripts and Stan code used to calibrate the syphilis transmission model to annual incidence data for Singapore and England </br>
* The doxypep_syphilis_projection folder includes R scripts to forward-simulate syphilis transmission dynamics under baseline conditions (without doxy-PEP) and various doxy-PEP prescribing strategies for Singapore and England </br>
* The scenarios folder contains R scripts for running alternative scenario analyses, including Singapore lower and upper incidence bounds, variations in adherence behavioural patterns, different screening frequencies, and assumptions regarding the stabilization or continuation of calibrated time-varying parameters </br>
* The plot folder provides R scripts to generate all figures included in the manuscript

![alt text](https://github.com/killingbear999/doxypep_syphilis/blob/main/doxyPEP_syphilis.png)
