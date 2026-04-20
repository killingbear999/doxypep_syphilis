# Long-term public health impact of doxycycline post-exposure prophylaxis on syphilis transmission

Zihao Wang, Dariya Nikitin, Borame L. Dickens, Liang En Wee, Martin T.W. Chio, Rayner Kay Jin Tan, Keisuke Ejima, Yi Wang, David N. Fisman, Lilith K. Whittles, Jue Tao Lim </br>
**Nature Health** </br>
Paper: https://doi.org/10.1038/s44360-026-00092-3 </br>

Requires: RStan (version 2.32.7), R (version 4.5.0), deSolve package (version 1.40) </br>

### File description
* All file names are self-explanatory: filenames ending with "_uk" refer to code for England, while filenames without the "_uk" suffix refer to code for Singapore </br>
* The syphilis_calibration folder contains R scripts and Stan code used to calibrate the syphilis transmission model to annual incidence data for Singapore and England </br>
* The doxypep_syphilis_projection folder includes R scripts to forward-simulate syphilis transmission dynamics under baseline conditions (without doxy-PEP) and various doxy-PEP prescribing strategies for Singapore and England </br>
* The scenarios folder contains R scripts for running alternative scenario analyses, including Singapore lower and upper incidence bounds, variations in adherence behavioural patterns, different screening frequencies, and assumptions regarding the stabilization or continuation of calibrated time-varying parameters </br>
* The plot folder provides R scripts to generate all figures included in the manuscript

### Objective
* Develops a behavioural transmission-dynamic model, calibrated using Bayesian methods with epidemiological and behavioural data from Singapore and England, to evaluate how different doxy-PEP enrolment strategies influence syphilis incidence over the long term
* Integrates trial-derived efficacy with real-world behavioural factors, such as screening frequency, uptake, adherence, and discontinuation, to provide a more realistic assessment of intervention impact

### Methodology
* Please refer to Supplementary Information from the publisher for full implementation details: https://static-content.springer.com/esm/art%3A10.1038%2Fs44360-026-00092-3/MediaObjects/44360_2026_92_MOESM1_ESM.pdf
  
![alt text](https://github.com/killingbear999/doxypep_syphilis/blob/main/doxyPEP_syphilis.png)

### Results
Our work highlights that effective implementation of doxy-PEP programmes should prioritise high-risk MSM (i.e., those with more than five sexual partners per year) at the point of syphilis diagnosis, combined with frequent STI screening (every 3-4 months), while closely monitoring the potential emergence of tetracycline-resistant N. gonorrhoeae.

## Citation </br>
If you find our work relevant to your research, please cite:
```
@article{wang2026long,
  title={Long-term public health impact of doxycycline post-exposure prophylaxis on syphilis transmission},
  author={Wang, Zihao and Nikitin, Dariya and Dickens, Borame L and Wee, Liang En and Chio, Martin TW and Tan, Rayner Kay Jin and Ejima, Keisuke and Wang, Yi and Fisman, David N and Whittles, Lilith K and others},
  journal={Nature Health},
  pages={1--13},
  year={2026},
  publisher={Nature Publishing Group UK London}
}
```
