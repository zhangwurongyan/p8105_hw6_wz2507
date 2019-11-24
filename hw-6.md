hw6
================
Wurongyan Zhang
11/20/2019

# Problem 1

## data cleaning

``` r
birthweight <- read_csv("data/birthweight.csv") %>% mutate(babysex = as.factor(babysex),
         frace = as.factor(frace),
         malform = as.factor(malform),
         mrace = as.factor(mrace)) %>% janitor::clean_names()

colSums(is.na(birthweight)) %>% knitr::kable()
```

|          | x |
| -------- | -: |
| babysex  | 0 |
| bhead    | 0 |
| blength  | 0 |
| bwt      | 0 |
| delwt    | 0 |
| fincome  | 0 |
| frace    | 0 |
| gaweeks  | 0 |
| malform  | 0 |
| menarche | 0 |
| mheight  | 0 |
| momage   | 0 |
| mrace    | 0 |
| parity   | 0 |
| pnumlbw  | 0 |
| pnumsga  | 0 |
| ppbmi    | 0 |
| ppwt     | 0 |
| smoken   | 0 |
| wtgain   | 0 |

From the summary above, we can see that there are no missing data. I
converted babysex, frace, malform and mrace to factor. The data set
contains 4342 observations with in total 20 variables.

## regression model

``` r
model1 = lm(bwt~., data = birthweight)
step(model1, direction = "both")
```

    ## Start:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + ppbmi + ppwt + smoken
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - frace     4    124365 320848704 48712
    ## - malform   1      1419 320725757 48716
    ## - ppbmi     1      6346 320730684 48716
    ## - momage    1     28661 320752999 48716
    ## - mheight   1     66886 320791224 48717
    ## - menarche  1    111679 320836018 48717
    ## - ppwt      1    131132 320855470 48718
    ## <none>                  320724338 48718
    ## - fincome   1    193454 320917792 48718
    ## - parity    1    413584 321137922 48721
    ## - mrace     3    868321 321592659 48724
    ## - babysex   1    853796 321578134 48727
    ## - gaweeks   1   4611823 325336161 48778
    ## - smoken    1   5076393 325800732 48784
    ## - delwt     1   8008891 328733230 48823
    ## - blength   1 102050296 422774634 49915
    ## - bhead     1 106535716 427260054 49961
    ## 
    ## Step:  AIC=48711.51
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - malform   1      1447 320850151 48710
    ## - ppbmi     1      6975 320855679 48710
    ## - momage    1     28379 320877083 48710
    ## - mheight   1     69502 320918206 48710
    ## - menarche  1    115708 320964411 48711
    ## - ppwt      1    133961 320982665 48711
    ## <none>                  320848704 48712
    ## - fincome   1    194405 321043108 48712
    ## - parity    1    414687 321263390 48715
    ## + frace     4    124365 320724338 48718
    ## - babysex   1    852133 321700837 48721
    ## - gaweeks   1   4625208 325473911 48772
    ## - smoken    1   5036389 325885093 48777
    ## - delwt     1   8013099 328861802 48817
    ## - mrace     3  13540415 334389119 48885
    ## - blength   1 101995688 422844392 49908
    ## - bhead     1 106662962 427511666 49956
    ## 
    ## Step:  AIC=48709.53
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + momage + mrace + parity + ppbmi + ppwt + 
    ##     smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - ppbmi     1      6928 320857079 48708
    ## - momage    1     28660 320878811 48708
    ## - mheight   1     69320 320919470 48708
    ## - menarche  1    116027 320966177 48709
    ## - ppwt      1    133894 320984044 48709
    ## <none>                  320850151 48710
    ## - fincome   1    193784 321043934 48710
    ## + malform   1      1447 320848704 48712
    ## - parity    1    414482 321264633 48713
    ## + frace     4    124393 320725757 48716
    ## - babysex   1    851279 321701430 48719
    ## - gaweeks   1   4624003 325474154 48770
    ## - smoken    1   5035195 325885346 48775
    ## - delwt     1   8029079 328879230 48815
    ## - mrace     3  13553320 334403471 48883
    ## - blength   1 102009225 422859375 49906
    ## - bhead     1 106675331 427525481 49954
    ## 
    ## Step:  AIC=48707.63
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + momage + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - momage    1     29211 320886290 48706
    ## - menarche  1    117635 320974714 48707
    ## <none>                  320857079 48708
    ## - fincome   1    195199 321052278 48708
    ## + ppbmi     1      6928 320850151 48710
    ## + malform   1      1400 320855679 48710
    ## - parity    1    412984 321270064 48711
    ## + frace     4    125020 320732060 48714
    ## - babysex   1    850020 321707099 48717
    ## - mheight   1   1078673 321935752 48720
    ## - ppwt      1   2934023 323791103 48745
    ## - gaweeks   1   4621504 325478583 48768
    ## - smoken    1   5039368 325896447 48773
    ## - delwt     1   8024939 328882018 48813
    ## - mrace     3  13551444 334408523 48881
    ## - blength   1 102018559 422875638 49904
    ## - bhead     1 106821342 427678421 49953
    ## 
    ## Step:  AIC=48706.02
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - menarche  1    100121 320986412 48705
    ## <none>                  320886290 48706
    ## - fincome   1    240800 321127090 48707
    ## + momage    1     29211 320857079 48708
    ## + ppbmi     1      7479 320878811 48708
    ## + malform   1      1678 320884612 48708
    ## - parity    1    431433 321317724 48710
    ## + frace     4    124743 320761547 48712
    ## - babysex   1    841278 321727568 48715
    ## - mheight   1   1076739 321963029 48719
    ## - ppwt      1   2913653 323799943 48743
    ## - gaweeks   1   4676469 325562760 48767
    ## - smoken    1   5045104 325931394 48772
    ## - delwt     1   8000672 328886962 48811
    ## - mrace     3  14667730 335554021 48894
    ## - blength   1 101990556 422876847 49902
    ## - bhead     1 106864308 427750598 49952
    ## 
    ## Step:  AIC=48705.38
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     mheight + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## <none>                  320986412 48705
    ## + menarche  1    100121 320886290 48706
    ## - fincome   1    245637 321232048 48707
    ## + momage    1     11698 320974714 48707
    ## + ppbmi     1      8823 320977589 48707
    ## + malform   1      1884 320984528 48707
    ## - parity    1    422770 321409181 48709
    ## + frace     4    128726 320857686 48712
    ## - babysex   1    846134 321832545 48715
    ## - mheight   1   1012240 321998651 48717
    ## - ppwt      1   2907049 323893461 48743
    ## - gaweeks   1   4662501 325648912 48766
    ## - smoken    1   5073849 326060260 48771
    ## - delwt     1   8137459 329123871 48812
    ## - mrace     3  14683609 335670021 48894
    ## - blength   1 102191779 423178191 49903
    ## - bhead     1 106779754 427766166 49950

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     gaweeks + mheight + mrace + parity + ppwt + smoken, data = birthweight)
    ## 
    ## Coefficients:
    ## (Intercept)     babysex2        bhead      blength        delwt  
    ##   -6098.822       28.558      130.777       74.947        4.107  
    ##     fincome      gaweeks      mheight       mrace2       mrace3  
    ##       0.318       11.592        6.594     -138.792      -74.887  
    ##      mrace4       parity         ppwt       smoken  
    ##    -100.678       96.305       -2.676       -4.843

The model chosen from stepwise regression (for both directions)
is:

``` r
swmodel=lm(bwt~babysex + bhead + blength + delwt + fincome + gaweeks + mheight + mrace + parity + ppwt + smoken, data = birthweight)
summary(swmodel)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     gaweeks + mheight + mrace + parity + ppwt + smoken, data = birthweight)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1097.18  -185.52    -3.39   174.14  2353.44 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6098.8219   137.5463 -44.340  < 2e-16 ***
    ## babysex2       28.5580     8.4549   3.378 0.000737 ***
    ## bhead         130.7770     3.4466  37.944  < 2e-16 ***
    ## blength        74.9471     2.0190  37.120  < 2e-16 ***
    ## delwt           4.1067     0.3921  10.475  < 2e-16 ***
    ## fincome         0.3180     0.1747   1.820 0.068844 .  
    ## gaweeks        11.5925     1.4621   7.929 2.79e-15 ***
    ## mheight         6.5940     1.7849   3.694 0.000223 ***
    ## mrace2       -138.7925     9.9071 -14.009  < 2e-16 ***
    ## mrace3        -74.8868    42.3146  -1.770 0.076837 .  
    ## mrace4       -100.6781    19.3247  -5.210 1.98e-07 ***
    ## parity         96.3047    40.3362   2.388 0.017004 *  
    ## ppwt           -2.6756     0.4274  -6.261 4.20e-10 ***
    ## smoken         -4.8434     0.5856  -8.271  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 272.3 on 4328 degrees of freedom
    ## Multiple R-squared:  0.7181, Adjusted R-squared:  0.7173 
    ## F-statistic: 848.1 on 13 and 4328 DF,  p-value: < 2.2e-16

The plot of model residuals against fitted values are shown below:

``` r
birthweight %>% 
  add_predictions(swmodel) %>% 
  add_residuals(swmodel) %>% ggplot(aes(x=pred, y=resid))+
  geom_point(color="pink") + geom_smooth(method = "lm")+labs(title = "residuals against fitted values", x="fitted values", y="residuals")
```

![](hw-6_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The model has \(R^2\) of 0.718 which means 71.8% of the variability of
birthweight was explained by the model. The relationship between
residuals and fitted values are roughly constant on the right side with
higher values but there are some random residuals on the left side with
lower values as we can see from the plot above.

## comparing models

In order to compare those three models, I would like to use cross
validation.

The three models are
\[stepwise: bwt\sim babysex + bhead + blength + delwt + fincome + gaweeks + mheight + mrace + parity + ppwt + smoken\\ main :bwt \sim gaweeks \\head:bwt \sim bhead + blength + babysex + bhead * blength * babysex\]

The summary of the other two models are shown below:

``` r
model_main = lm(bwt ~ gaweeks ,data = birthweight)
summary(model_main)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ gaweeks, data = birthweight)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1730.52  -292.85    -0.78   303.47  1591.36 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  476.003     88.809    5.36 8.76e-08 ***
    ## gaweeks       66.920      2.245   29.80  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 466.7 on 4340 degrees of freedom
    ## Multiple R-squared:  0.1699, Adjusted R-squared:  0.1697 
    ## F-statistic: 888.3 on 1 and 4340 DF,  p-value: < 2.2e-16

``` r
model_head = lm(bwt ~ bhead + blength + babysex + bhead * blength * babysex ,data = birthweight)
summary(model_head)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ bhead + blength + babysex + bhead * blength * 
    ##     babysex, data = birthweight)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1132.99  -190.42   -10.33   178.63  2617.96 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            -7176.8170  1264.8397  -5.674 1.49e-08 ***
    ## bhead                    181.7956    38.0542   4.777 1.84e-06 ***
    ## blength                  102.1269    26.2118   3.896 9.92e-05 ***
    ## babysex2                6374.8684  1677.7669   3.800 0.000147 ***
    ## bhead:blength             -0.5536     0.7802  -0.710 0.478012    
    ## bhead:babysex2          -198.3932    51.0917  -3.883 0.000105 ***
    ## blength:babysex2        -123.7729    35.1185  -3.524 0.000429 ***
    ## bhead:blength:babysex2     3.8781     1.0566   3.670 0.000245 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 287.7 on 4334 degrees of freedom
    ## Multiple R-squared:  0.6849, Adjusted R-squared:  0.6844 
    ## F-statistic:  1346 on 7 and 4334 DF,  p-value: < 2.2e-16

``` r
set.seed(100)

cv_bw = birthweight %>% crossv_mc(100) %>% mutate(train = map(train, as_tibble), test = map(test, as_tibble))

cv_bw = cv_bw %>% mutate(
  stepwise = map(train, ~lm(bwt ~babysex + bhead + blength +delwt +fincome + gaweeks + mheight + mrace + parity + ppwt + smoken, data = .)),
  main = map(train, ~lm(bwt ~ gaweeks, data=.)),
  head = map(train, ~lm(bwt ~ bhead + blength + babysex + bhead * blength * babysex ,data =.))
) %>% mutate(rmse_stepwise = map2_dbl(stepwise, test, ~rmse(model = .x, data = .y)),
             rmse_main = map2_dbl(main, test, ~rmse(model = .x, data = .y)),
             rmse_head= map2_dbl(head, test, ~rmse(model = .x, data = .y)))

cv_bw %>% select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse, color=model)) + geom_violin() + labs(title = "comparison of the models")
```

![](hw-6_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

From the violin plot, we can see that the model from stepwise selection
has the lowest rmse among those three models so the this model has
better fit compare to the other two. For the comparison between main
effect model and the interaction model, the interaction model has lower
rmse than the other one.

# Problem 2

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

### bootstrap

``` r
set.seed(100)

results = weather_df %>% 
  bootstrap(5000) %>% mutate(model = map(strap, ~lm(tmax~tmin, data = .x)),coefficient = map(model, broom::tidy),  summary = map(model, broom::glance)) %>% 
  select(-strap,-model) %>% unnest(summary, coefficient) %>% mutate(term=recode(term, "(Intercept)" = "beta0", "tmin"="beta1")) %>%
  select(r.squared, adj.r.squared, term, estimate)  %>%  pivot_wider(names_from = term, values_from = estimate) %>% mutate(log = log(beta0*beta1))

results[1:5,] %>% 
  select(r.squared, adj.r.squared,log) %>% knitr::kable()
```

| r.squared | adj.r.squared |      log |
| --------: | ------------: | -------: |
| 0.9033037 |     0.9030373 | 2.025275 |
| 0.8870431 |     0.8867319 | 2.044843 |
| 0.9187228 |     0.9184989 | 1.999614 |
| 0.9158852 |     0.9156535 | 1.999788 |
| 0.9245112 |     0.9243032 | 2.020667 |

We can see from the glance of the data set that the data contains our
main interest of \(\hatR^2\) and
\(log(\hat\beta_0 * \hat\beta_1)\).

## distribution of log beta

``` r
results %>% ggplot(aes(x = log)) + geom_density(fill = "pink") +labs(title = "distribution of log(beta0*beta1)",x = "log(beta0*beta1)")
```

![](hw-6_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
quantile(pull(results,log),c(0.025,0.975))
```

    ##     2.5%    97.5% 
    ## 1.964370 2.057061

The 95% confidence interval for \(log(\hat\beta_0 * \hat\beta_1)\) is
between (1.964, 2.057). From the plot we can see that the distribution
is kind of normal with a little longer tail on the left
side.

## distribution of r-squared

``` r
results %>% ggplot(aes(x = r.squared)) + geom_density(fill = "grey") +labs(title = "distribution of r squared",x = "r squared")
```

![](hw-6_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
quantile(pull(results,r.squared),c(0.025,0.975))
```

    ##      2.5%     97.5% 
    ## 0.8938654 0.9271376

The 95% confidence interval for \(R^2\) is between (0.894, 0.927). From
the plot we can see that the distribution is kind of normal and a little
bit right skewed.
