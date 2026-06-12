# Collecting Prosody in the Wild: A Content-Controlled, Privacy-First Smartphone Protocol and Empirical Evaluation

This repository contains analysis scripts and reproducibility materials accompanying the paper:
“Collecting Prosody in the Wild: A Content-Controlled, Privacy-First Smartphone Protocol and Empirical Evaluation”.

The paper introduces and empirically evaluates a content-controlled, privacy-first smartphone protocol for collecting prosodic speech data in everyday life. The protocol uses scripted read-aloud sentences, extracts acoustic features on the participant’s device using openSMILE, deletes raw audio immediately, and transmits only derived feature-level data for analysis.

This repository provides materials for reproducing the statistics reported in the empirical evaluation, including scripts for data preprocessing, filtering, descriptive analyses, acoustic diagnostics, condition contrasts, and prediction analyses.

A demonstration implementation of the on-device audio processing pipeline as an Android module is available here:
https://github.com/Flo890/demo-prosody-in-the-wild

If you use materials from this repository, please cite the accompanying preprint:

Koch, T. K., Bemmann, F., Schoedel, R., Buehner, M., & Stachl, C. (2026). Collecting Prosody in the Wild: A Content-Controlled, Privacy-First Smartphone Protocol and Empirical Evaluation. arXiv preprint arXiv:2603.17061. Accepted at Interspeech 2026.
