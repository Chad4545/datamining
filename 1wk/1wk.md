# Datamining 1wk homework

### centering, scaling, standardization 으로 인한 coefficients 해석에 대한 변화

- Scaling -  scaling은 각 예측변수들의 scale이 지나치게 다른경우 계수들의 scale을 통일시켜 scale이 작은 변수들의 기여를 제대로 표현하고자 하기 위함이다.  또 큰 scale에서 오는 버림오차 등에 대한 위험을 줄이고자 실행한다. 또한 수학적으로는 descent 알고리즘에서 가파른 기울기를 완화시켜주는 의미를 갖는다. 또한 보통 반응변수는 scale대상이 아니다.

-  정리하자면 모든 변수들의 mean을 0으로 만들어 절편항에 대한 의미를 부여하는 것 등의 목적으로 행하는 것이 centering이고, scale의 차이로 인한 계수해석의 어려움을 방지하고자 행하는 것이 scaling이다. 다항회귀가 아닌경우, 두가지 모두 분석 자체에 영향을 미치지 않는다. centering하는것과 scaling이 되는것을 통한 그 밖의 여러 효과가 있다

- 추가로, scaling이 되었다고 회귀계수들을 바로 변수들의 중요성으로 보는 것은 위험한데, 이는 각 예측변수들이 서로 correlated되어 있을 경우 각 회귀계수들이 다른 예측변수들에도 영향을 받기때문이다. 

---

## centering 

- In centering, you are changing the values but not the scale.  So a predictor that is centered at the mean has new values–the entire scale has shifted so that the mean now has a value of 0, but one unit is still one unit.  The intercept will change, but the regression coefficient for that variable will not.  Since the regression coefficient is interpreted as the effect on the mean of Y for each one unit difference in X, it doesn’t change when X is centered.

- And incidentally, despite the name, you don’t have to center at the mean.  It is often convenient, but there can be advantages of choosing a more meaningful value that is also toward the center of the scale.

---

##  standardization

