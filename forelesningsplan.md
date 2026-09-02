# Course Plan SOK-3081 Econometrics Fall 2026

| Session  | Date | To do                                            |
|------------------|---------|------------------------------------------------------------------|
| *Preparation*    |         | We use the open-source programming software R, along with its graphical user interface, RStudio, for all the estimations. We highly recommend you download these onto your personal computer and spend some time acquainting yourself with the RStudio interface. Download R from [here](https://cloud.r-project.org/){:target="blank"}, and RStudio from [here](https://posit.co/download/rstudio-desktop/){:target="blank"}. Alternatively, you can access R through the Jupyter notebook interface using your web-browser [here](https://jupyter.uit.no){:target="blank"}. Use your UiT username and password.   |
|                  |         | No knowledge of R up front. Follow the pre-course in [datacamp](https://app.datacamp.com/groups/sok-3081-econometrics-principles/dashboard).        |
|                  |          | During the semester, use a LLM to learn how to use R. You can also search Youtube for "learning r". Google has some introductory videos [here](https://www.youtube.com/playlist?list=PLOU2XLYxmsIK9qQfztXeybpHvru-TrqAP){:target='blank'}. Follow this link for a bunch of [Cheatsheets](https://posit.co/resources/cheatsheets/){:target='blank'}. Note the [RStudio IDE :: Cheatsheet](https://rstudio.github.io/cheatsheets/html/rstudio-ide.html){:target='blank'} and the [Data Transformation with dplyr](https://rstudio.github.io/cheatsheets/html/data-transformation.html) cheatsheet. Our textbook is: [Principles of Econometrics](http://principlesofeconometrics.com/poe5/poe5.html){:target='blank'}, 5th Edition (POE5), Wiley - 2018. Available at the campus bookstore Akademika. Buy the book! You will not manage the course without it. |
| **Resources** |            | The 4th edition of our textbook has an [online version with R code](https://bookdown.org/ccolonescu/RPoE4/){:target='_blank_'}. There is an online book called [Introduction to econometrics with R](https://www.econometrics-with-r.org/index.html){:target='_blank_'}. Another online book is [Using R for Introductory Econometrics](http://urfie.net){:target='_blank_'}. Also available for Python and Julia. When you are on the UiT network, you have access to [Applied Econometrics with R](https://link.springer.com/book/10.1007%2F978-0-387-77318-6){:target='_blank_'}. They have a [web-page](https://eeecon.uibk.ac.at/~zeileis/teaching/AER/){:target='_blank_'}. Springer has a whole series of [books in the Use R! section](https://link.springer.com/search?facet-series=%226991%22&facet-content-type=%22Book%22){:target='_blank_'}. |
|                  |         | The ultimate resource for online books on R is [The big book of R](https://www.bigbookofr.com/){:target='_blank_'}. |
| **Self Study** |     |  Probability Primer, POE5 p. 15-39 |
|   |   |  Example P.1 Using a cdf  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p1%20using%20a%20cdf.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=20d5f48a-e081-4ce0-b024-b0610078f235){:target='_blank_'}  |
|   |   | Example P.2 Calculating a conditional probability [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p2%20calculating%20a%20conditional%20probability.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=ae78fd28-9635-41fe-a3a9-b0610078f247){:target='_blank_'}  [Intuition behind (video)](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=79a7b549-d8d7-4230-87fd-b0610078f23f){:target='_blank_'}  |  
|   |   | Example P.3 Calculating an expected value  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p3%20calculating%20an%20expected%20value.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=32f60538-9af0-4ea4-89f3-b0610078f0d9){:target='_blank_'}   |  
|   |   | Example P.4 Calculating a Conditional Expectation  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p4%20calculating%20a%20conditional%20expectation.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=7f00a56e-92b0-4c21-8073-b0610078f577){:target='_blank_'}   |  
|   |   | Example P.5 Calculating a Variance  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p5%20calculating%20a%20variance.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=d75b1ca0-b337-4438-b4b5-b0610078f64c){:target='_blank_'}   |  
|   |   | Example P.6 Calculating a Correlation [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p6%20calculating%20a%20correlation.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=64831e5e-8ca8-41db-b18b-b0610078f747){:target='_blank_'} [Correlation is a standardized covariance (video)](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=068b0fb1-4f62-49c6-86f7-b0610078f56e){:target='_blank_'}   |  
|   |   | Example P.7 Conditional Expectation  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p7%20conditional%20expectation.R){:target='_blank_'}     |  
|   |   | Example P.8 Conditional Variance  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p8%20conditional%20variance.R){:target='_blank_'}     |  
|   |   | Example P.9 Iterated Expectation  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p9%20iterated%20expectation.R){:target='_blank_'}     |  
|   |   | Example P.10 Covariance Decomposition  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p10%20covariance%20decomposition.R){:target='_blank_'}   |  
|   |   | Example P.11 Normal Distribution Probability Calculation  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p11-normal-distribution-probability-calculation.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=ed10162c-003a-441f-bc7f-b06200d77a64){:target='_blank_'}   |  
|   |    | **Introduction to R** |
|    |       |   Download and [install R (first) then RStudio](https://posit.co/download/rstudio-desktop/){:target='_blank_'} |
|    |       |  [RStudio IDE :: Cheatsheet](https://rstudio.github.io/cheatsheets/html/rstudio-ide.html){:target='_blank_'}   |
|    |       |  Web alternative: <https://jupyter.uit.no> R environment | 
|    |       |  [Data files from Principles of Econometrics](https://www.principlesofeconometrics.com/poe5/poe5data.html){:target='blank'}    |
|    |    | Browse the paper “Econometric Computing with R” (in Files/Readings in Canvas) |
|  1 | 17/08 | Chapter 1. [An introduction to econometrics](https://uit-sok-3081-h26.github.io/lecture-00.html)  |
|    |       |  [R file](https://raw.githubusercontent.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/refs/heads/main/lecture-00.R)     |
|    |       |  [My scribbling notes](https://uit-sok-3081-h26.github.io/Notat%2017.%20aug.%202026.pdf) |
|  2 | 18/08  | Appendix A: [Mathematical Tools in R](https://uit-sok-3081-h26.github.io/lecture-01.html)   |
|    |        | [R file](https://raw.githubusercontent.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/refs/heads/main/lecture-01.R)      |
|    |        | [Annotated Quarto file for Appendix A](https://github.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/blob/main/Apdx_A_annotated_v2.qmd)   |
|    |       |  [My scribbling notes](https://uit-sok-3081-h26.github.io/Notat%2018.%20aug.%202026.pdf) |
| 3  | 19/08    | Appendix B: [Probability Concepts](https://uit-sok-3081-h26.github.io/lecture-02.html)   |
|    |      |    [R file](https://raw.githubusercontent.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/refs/heads/main/lecture-02.R)           |
|    |        | [Annotated Quarto file for Appendix B](https://github.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/blob/main/Apdx_B_annotated.qmd)   |
|    |       |  [My scribbling notes](https://uit-sok-3081-h26.github.io/Notat%2019.%20aug.%202026.pdf) |
|    |       |  [My scribbling notes on the html file](https://uit-sok-3081-h26.github.io/Lecture%2002_%20Probability%20for%20Econometrics%20(Appendix%20B)%20scribling.pdf) |
| 4 | 20/08    |  Appendix C: [Review of Statistical Inference - Part 1](https://uit-sok-3081-h26.github.io/lecture-03.html)     |
|    |      |    [R file](https://raw.githubusercontent.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/refs/heads/main/lecture-03.R)           |
|    |       |  [My scribbling notes](https://uit-sok-3081-h26.github.io/Notat%2020.%20aug.%202026.pdf) |
|    |       |  [Handout: Why We Use the Sample Mean](https://uit-sok-3081-h26.github.io/mean_as_minimum_variance_estimator.pdf) |
| 5 | 31/08    |  Appendix C: [Review of Statistical Inference - Part 2](https://uit-sok-3081-h26.github.io/lecture-04.html)    |
|    |      |    [R file](https://raw.githubusercontent.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/refs/heads/main/lecture-04.R)           |
|    |        | [Annotated Quarto file for Appendix C](https://github.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/blob/main/Appendix_C_annotated.qmd)   |
|    |       |  [My scribbling notes](https://uit-sok-3081-h26.github.io/Notat%2031.%20aug.%202026.pdf) |
|    |        |  [Self study on p-values](https://uit-sok-3081-h26.github.io/p-values-self-study.html) |
| 6 |  01/09   | [Live R coding seminar](https://raw.githubusercontent.com/uit-sok-3081-h26/uit-sok-3081-h26.github.io/refs/heads/main/econometrics_llm_lab_tasks.R)    |
|    |       |  [My scribbling notes](https://uit-sok-3081-h26.github.io/Notat%201.%20sep.%202026.pdf) |
| 7 | 03/09   | Chapter 2   |
| 8 | 09/09   | Chapter 3   |
| 9 | 11/09   | Chapter 4   |
| 10 | 14/09  | Chapter 5    |
| 11 | 14/09   | Seminar   |
| Exercise 1  | Deadline 16/09   |   [Obligatory Exercise 1](https://uit-sok-3081-h26.github.io/SOK-3081_Turkish_lira_inflation_html.html). Submit your paper as a pdf in Canvas.    |
| 12 | 18/09   | Chapter 6   |
| 13 | 22/09   | Chapter 7   |
| 14 | 25/09   | Chapter 8   |
| 15 | 30/09   | Seminar  |
|  |    |    |
