# FLUXNETGranger
Code and scripts to support the AmeriFlux webinar "Exploring Modern Timeseries Techniques with FLUXNET data"

## Presentation
- [Video of Presentation](https://youtu.be/I11U0ZnBXSw)
- [PDF of Presentation](https://drive.google.com/file/d/1Eoc9YjtO0uEc8LtNSLYUaYR9PJsIQplU/view?usp=sharing)

## References
- Altman, N., and M. Krzywinski (2015), Association, correlation and causation, Nature Methods, 12(10), 899–900, [doi:10.1038/nmeth.3587](doi:10.1038/nmeth.3587).
- Kalisch, M., M. Mächler, D. Colombo, M. H. Maathuis, and P. Bühlmann (2012), Causal Inference Using Graphical Models with the R Package pcalg, Journal of Statistical Software, 47(1), 1–26, (doi:10.18637/jss.v047.i11)[doi:10.18637/jss.v047.i11].
- Ombadi, M., P. Nguyen, S. Sorooshian, and K. Hsu (2020), Evaluation of Methods for Causal Discovery in Hydrometeorological Systems, Water Resources Research, 56(7), e2020WR027251, (doi:10.1029/2020WR027251)[doi:10.1029/2020WR027251](doi:10.1029/2020WR027251).
- Runge, J. et al. (2019), Inferring causation from time series in Earth system sciences, Nat Commun, 10(1), 2553, [doi:10.1038/s41467-019-10105-3](doi:10.1038/s41467-019-10105-3).
- Sugihara, G., R. May, H. Ye, C. Hsieh, E. Deyle, M. Fogarty, and S. Munch (2012), Detecting Causality in Complex Ecosystems, Science, 338(6106), 496–500, [doi:10.1126/science.1227079](doi:10.1126/science.1227079).

## Links to R Packages
- [RTransferEntropy](https://cran.r-project.org/web/packages/RTransferEntropy/index.html): Transfer Entropy
- [rEDM](https://cran.r-project.org/web/packages/rEDM/index.html): Convergent Cross Mapping
- [pcALG](https://cran.r-project.org/web/packages/pcalg/index.html): PC Algorithm (not shown during presentation)

## Installation 
Installation in R is done through the devtools package:

`install_github("jmzobitz/FLUXNETGranger", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"),force=TRUE)`

If you encounter problems with code in this repository, feel free to post an [issue](https://github.com/jmzobitz/FLUXNETGranger/issues).

## License
This work is distributed under the Creative Commons, Attribution-Non Commercial 4.0 License.  You may copy, distribute, display and perform the work and make derivative works and remixes based on it only if they give the author (Zobitz) attribution and use it for non-commerical purposes.  Please inform the author (by posting an [issue](https://github.com/jmzobitz/FLUXNETGranger/issues) if he mistakenly omitted attribution in the code.
