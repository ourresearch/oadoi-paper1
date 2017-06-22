OA paper
================
many

*very much a work in progress. we're committing to this regularly.*

Results outline so far: - How accurate is our OA detection (from Juan, modified to use hybrid analysis) - How much OA is there? - How is open access changing over time? - How do OA patterns vary across publishers? - Which repositories contribute most to OA availability? - How do OA patterns vary by discipline? - How much OA is there in most highly-accessed papers? - Do different types of OA have different citation patterns?

\# How much OA is there?
========================

``` r
articles_all %>% count(oa) %>% mutate(proportion=n/sum(n))
```

    ## # A tibble: 6 × 3
    ##            oa     n   proportion
    ##        <fctr> <int>        <dbl>
    ## 1      closed 72029 0.7214081827
    ## 2   gold_free 16202 0.1622715209
    ## 3 gold_hybrid  3586 0.0359156693
    ## 4   gold_doaj  3183 0.0318794131
    ## 5  green_only  4816 0.0482347639
    ## 6          NA    29 0.0002904502

``` r
kable(articles_all %>% count(oa) %>% mutate(percent=round(100*n/sum(n),1)))
```

| oa           |      n|  percent|
|:-------------|------:|--------:|
| closed       |  72029|     72.1|
| gold\_free   |  16202|     16.2|
| gold\_hybrid |   3586|      3.6|
| gold\_doaj   |   3183|      3.2|
| green\_only  |   4816|      4.8|
| NA           |     29|      0.0|

Category definitions:

-   *closed*: We could not find a free fulltext copy.
-   *gold\_free*: Free-to-read on the publisher page, with no license we could find.
-   *gold\_hybrid*: Free-to-read on the publisher page, published under some kind of open license.
-   *gold\_doaj* Free-to-read on the publisher page, and listed as open according to the DOAJ.
-   *green\_only*: The article is Green OA. We couldn't find any free copy on the publisher page, but we did find one in a repository. Note: this category is for copies that are *only* available in the repository, nowhere else.
-   *NA*: Processing error of som kind...we'll fix these before publication

So, about 28% of the DOI-assigned literature is available to read. Given that there are 66,560,153 total journal articles with a Crossref DOI (from <http://api.crossref.org/works?filter=type:journal-article>), that means we can estimate there are *at least* 66560153 \* 0.279 = 18570283 free-to-read articles (18.6 million) with Crossref DOIs.

But we know that in recent years OA has been gaining steam, so let's let's look more closely at OA over time.

\# How is open access changing over time?
=========================================

How complete is the publication year data from Crossref?

``` r
articles_all %>% group_by(year <= 2017 & year >= 1500) %>% summarise(n())
```

    ## # A tibble: 3 × 2
    ##   `year <= 2017 & year >= 1500` `n()`
    ##                           <lgl> <int>
    ## 1                         FALSE     4
    ## 2                          TRUE 99411
    ## 3                            NA   430

It seems the year data is pretty good, with less than 0.1% missing or obviously wrong years. We don't really want to look at data since 1500, so let's see what's a reasonable window to examine. We'll try 1950 because it's well before the "modern era" of open access.

``` r
# subset by time
articles_all %>% filter(year >= 1950 & year <= 2017) %>%
    ggplot(aes(x=year)) + geom_bar(width=1) 
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

The DOI sample was taken in early 2017, so unsurprisingly, we do not yet have enough DOIs from 2017 to plot. More surprisingly, 2016 seems to be underrepresented as well. Publishers can be slow to deposit information with Crossref, and this is likely the cause. So, we'll remove 2017 and 2016 from our timeseries subset.

Here's the version from 1950-2015:

``` r
articles_all = articles_all %>% mutate(is_modern = year >= 1950 & year <= 2015)

articles_all %>% count(is_modern) %>% mutate(proportion = n / sum(n))
```

    ## # A tibble: 3 × 3
    ##   is_modern     n  proportion
    ##       <lgl> <int>       <dbl>
    ## 1     FALSE 10691 0.107075968
    ## 2      TRUE 88724 0.888617357
    ## 3        NA   430 0.004306675

This modern subset will 89% of all DOIs ever, while letting us zoom in on the years of interest.

We'll start with plotting absolute numbers of OA articles:

``` r
articles_all %>% filter(is_modern) %>%
    ggplot(aes(x=year, fill=oa)) + geom_bar(width=1) + oa_color_map
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

We can see the absolute number of free-to-read articles of all kinds is growing significantly. However, we're particularly interested in the by-year *proportion* of the literature that is free to read.

``` r
# see http://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
oa_freq_by_year = articles_all %>% filter(is_modern) %>% count(year, oa) %>%  
  mutate(perc = n / sum(n)) %>%  
  ungroup()  

oa_freq_by_year %>% ggplot(aes(x=year, y=perc, fill=oa)) + geom_area() + oa_color_map
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

The proportion of OA is growing, too--not just the absolute amounts. This is driven the three Gold categories, all three of which are increasing over time. The percentage of Green Only OA is showing modest declines. Note that this doesn't necessarily reflect the number of papers actually being deposited, since this category is for papers available *only* in a repository. Gold-and-green articles are categorized as Gold. This means that growth in Gold may be disguising growth in the percentage of self-archived papers. We'll look more into this possibility further below. For now, thought, we turn to publishers.

\# Publishers
=============

It's interesting to see how openness looks when broken down by publisher. To do this, we'll subset the years more tightly and just look at articles since 2009. This will let us look at the more up-to-date picture that may include OA publishers.

``` r
articles_recent = articles_all %>% filter(is_modern, year >= 2009)
articles_recent$publisher = fct_infreq(articles_recent$publisher)

publishers = articles_recent %>% count(publisher) %>%
  ungroup()

# the top 20 publishers publish 82% of articles.
sum(publishers$n[0:82]) /sum(publishers$n)
```

    ## [1] 0.8194016

``` r
publishers_oa = articles_recent %>% 
  count(publisher, oa) %>%
  ungroup()


publishers_oa %>% slice(1:82) %>% mutate(publisher_rev=fct_rev(publisher)) %>% ggplot(aes(x=publisher_rev, y=n, fill=oa)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  oa_color_map
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
#same thing but by % oa

publishers_oa %>% slice(1:82) %>% mutate(publisher_rev=fct_rev(publisher)) %>% ggplot(aes(x=publisher_rev, y=n, fill=oa)) + 
  geom_bar(stat="identity", position="fill") + 
  coord_flip() +
  oa_color_map
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-7-2.png)

From this we can see that Elsevier is massively outpublishing anyone else. Because of this, they are also publishing more open-access content than any other single publisher. However, in percentage terms other publshers are doing much better at making content open. Unsurprisingly, the American Physical Society stands out as having more Green OA than other publishers in percentage terms, because of the prevelance of the ArXiv in Physics. PLOS stands out as the only open-access-only publisher in the list of top publishers.

\# Repositories
===============

+A different question is to dig into which repositories are contributing to making papers available. Let's take a look at PubMed Central in particular, since it has become the most important single source of Green OA. We'll lump all the other repositories (about 5000 of them) in a separate category and plot the results. As a reminder, we are only looking here at articles that are *only* available from a green repository:

We can see in the graphs at

``` r
articles_all = articles_all %>% mutate(base_collection_string=as.character(green_base_collections))

articles_all$repo = NULL
articles_all$repo[articles_all$oa=="green_only"] = "other"
articles_all$repo[grepl('/pmc/', articles_all$best_open_url)] = "PMC"
articles_all$repo[grepl('arxiv.org', articles_all$best_open_url)] = "arXiv"
articles_all$repo[grepl('.edu', articles_all$best_open_url)] = ".edu"

repo_ordered_levels = c("PMC", "arXiv", ".edu", "other")
articles_all = mutate(articles_all, repo=factor(repo, levels=repo_ordered_levels))

articles_all %>% filter(is_modern) %>% filter(!is.na(repo)) %>% ggplot(aes(x=year, fill=repo)) + geom_bar(width=1) + scale_fill_brewer(palette="Set3")
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-8-1.png)

It apprears that multi-year embargoes maybe affecting PMC, since the number of articles shows a surprising drop in the last few years. However despite this, we see that PMC remains by far the most significant green repository, particularly for papers published in the last decade.

``` r
articles_all %>% filter(is_modern, oa=="green_only") %>% count(repo) %>% mutate(proportion=n/sum(n))
```

    ## # A tibble: 4 × 3
    ##     repo     n proportion
    ##   <fctr> <int>      <dbl>
    ## 1    PMC  1315  0.3103611
    ## 2  arXiv   651  0.1536464
    ## 3   .edu  1161  0.2740146
    ## 4  other  1110  0.2619778

``` r
kable(articles_all %>% filter(is_modern, oa=="green_only") %>% count(repo) %>% mutate(percent=round(100*n/sum(n),1)))
```

| repo  |     n|  percent|
|:------|-----:|--------:|
| PMC   |  1315|     31.0|
| arXiv |   651|     15.4|
| .edu  |  1161|     27.4|
| other |  1110|     26.2|

That said, smaller repositories are still making a significant contribution to Green OA, particularly in recent years. for articles published since 2009, the contribute about as much as PMC (42%).

Growth in literature over time with any green
---------------------------------------------

We are only counting something as "green" if it's not available in any other format (Gold, hybrid). However, it's also interesting to look at how many articles are available in a repository, regardless of where else they might be open. Let's take a look at that below:

``` r
gray_green_color_map = scale_fill_manual(values=c("#777777", "#008000", "#FFD700"))

articles_all %>% filter(is_modern) %>% ggplot(aes(x=year, fill=found_green)) + geom_bar(width=1) + gray_green_color_map
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-10-1.png) As a proportion of all articles, deposits into repositories has been going up, with a recent drop. Embargos probaby play a large part in this, though deposit into places like ResearchGate (not included in our repository numbers) rather than institutional repositories may as well.

``` r
found_green_freq_by_year = articles_all %>% filter(is_modern) %>% count(year, found_green) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup()
found_green_freq_by_year %>% ggplot(aes(x=year, y=perc, fill=found_green)) + geom_area() + gray_green_color_map
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-11-1.png)

\# By license
=============

What are the most common licenses for open-access papers?

``` r
articles_all %>% filter(year >= 2009 & year <= 2015) %>% filter(grepl('cc', license)) %>% ggplot(aes(x=year, fill=license)) + geom_bar(width=1, position="fill") + scale_fill_brewer(palette="Set3")
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-12-1.png)

It looks like there has been steady growth in the number of articles licensed with the CC-BY license, largely at the expense of the CC-BY-NC license.

\# How much OA is there for most-accessed papers?
=================================================

DOIs accessed through Unpaywall during the week of XXX XX accesses, XXX unique DOIs, XXXX unique IP addresses. Selected random accesses until had 100k distinct DOIs

NOTE THIS GOES UP TO 2017, SEEMS RELEVANT

``` r
articles_accessed_raw <- read.csv("export_study_dois_unpaywall_accesses.csv")
articles_accessed = articles_accessed_raw 
articles_accessed = mutate(articles_accessed, oa=factor(oa_color_long, levels=oa_ordered_levels))
articles_accessed = articles_accessed %>% filter(!is.na(oa))
# how much oa
kable(articles_accessed %>% count(oa) %>% mutate(percent=round(100*n/sum(n),1)))
```

| oa           |      n|  percent|
|:-------------|------:|--------:|
| closed       |  55944|     53.0|
| gold\_free   |  16136|     15.3|
| gold\_hybrid |   8789|      8.3|
| gold\_doaj   |  15085|     14.3|
| green\_only  |   9661|      9.1|

``` r
articles_accessed %>% filter(!is.na(oa)) %>% ggplot(aes(x="", fill=oa)) + 
  geom_bar(position="fill", width=0.2) + 
  coord_flip() +
  oa_color_map 
```

![](oa_analysis_files/figure-markdown_github/unnamed-chunk-13-1.png)

\# OA and citation patterns
===========================

put steffi and vincent's stuff in here.
