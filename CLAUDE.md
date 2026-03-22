# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Course assignments for Prof. Qiang Chen's (陈强) textbook *Machine Learning and R Applications* (《机器学习及R应用》), completed in RStudio. Chapters 1–3 are concept-only; code starts at Chapter 4.

## Repository Structure

```
code/
  zh/   — original scripts with Chinese comments and variable names
  en/   — translated versions with English comments and variable names
```

## Running Scripts

Scripts are standalone R files, run in RStudio or via the R CLI:

```bash
Rscript "code/en/4.Linear_Regression.R"
Rscript "code/zh/4.线性回归.R"
```

Each script begins with `rm(list=ls())` to clear the workspace. Some scripts (Ch. 15) reference a local `setwd()` path that must be updated to match the local data directory before running.

## Chapter-to-File Mapping

| Chapter | Chinese file | English file | Topic |
|---|---|---|---|
| 4 | 4.线性回归.R | 4.Linear_Regression.R | Linear Regression |
| 5 | 5.逻辑回归.R | 5.Logistic_Regression.R | Logistic Regression |
| 6 | 6.多项逻辑回归.R | 6.Multinomial_Logistic_Regression.R | Multinomial Logistic Regression |
| 7 | 7.判别分析.R | 7.Discriminant_Analysis.R | Discriminant Analysis |
| 8 | 8.朴素贝叶斯.R | 8.Naive_Bayes.R | Naive Bayes |
| 9 | 9.惩罚回归.R | 9.Penalized_Regression.R | Penalized Regression (LASSO/Ridge/Elastic Net) |
| 10 | 10.K近邻法.R | 10.KNN.R | K-Nearest Neighbors |
| 11 | 11.决策树.R | 11.Decision_Tree.R | Decision Trees |
| 12 | 12.随机森林.R | 12.Random_Forest.R | Random Forests |
| 13 | 13.提升法.R | 13.Boosting.R | Gradient Boosting |
| 14 | 14.支持向量机.R | 14.SVM.R | Support Vector Machines |
| 15 | 15.人工神经网络.R | 15.Neural_Network.R | Neural Networks |
| 16 | 16.主成分分析.R | 16.PCA.R | PCA |
| 17 | 17.聚类分析.R | 17.Clustering.R | Clustering |
| 18 | 18.数据科学中的R语言.R | 18.R_for_Data_Science.R | R for Data Science (tidyverse) |

## Code Conventions

- Chinese versions use Chinese variable names (e.g., `训练集`, `测试集`, `预测`) and Chinese comments
- English versions translate both variable names and comments; logic is identical
- Datasets come from R packages (`AppliedPredictiveModeling`, `mlbench`, `nycflights13`) or local CSV files
- Train/test splits use `set.seed()` for reproducibility
- Each script is self-contained per chapter exercise
