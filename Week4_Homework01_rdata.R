#宿題

#1 ポアソン分布の最尤推定
x<-c(1, 4, 3, 3, 1, 0, 2, 2, 1, 2, 1, 1, 1, 3, 1)

#尤度関数
tencho_likelihood<-function(lambda){
  L <- 1
  m <-length(x)
  for(i in 1:m){
    L <- L * dpois(x[i], lambda = lambda)
  }
  return(L)
}

#最尤推定
optimise(tencho_likelihood, interval = c(0,15), maximum = TRUE)



#2 平均の差の検定
a<-c(0.91, 1.02, 1.13, 1.43, 1.67, 1.71)
b<-c(1.13, 1.44, 1.51, 1.66)

t.test(a, b, alternative = "greater")



#3 独立性の検定
x<-matrix(c(50, 30,
            25, 35), ncol=2, byrow=T)



#4の問題を解く前に以下を読んでください
#例題.正規分布の最尤推定 
#データはこちら
x<-rnorm(1000,100,3)

#尤度関数を定義します
likelihood_func<-function(x, y){
  L<-0
  for(i in 1:length(x)){
    L<- L + log(dnorm(x[i], mean = y[1], sd = y[2]))
  }
  L
}

#ポアソンはパラメーターが1つだったのでoptimise関数を使いましたが、
#正規分布はパラメーターが2つあるので、optim関数を使います。
# optimise関数は2つのパラメータを扱うことができないためです。

optim(par = c(90,1),  #パラメーターの初期値
      fn = likelihood_func, #定義した尤度関数
      x=x,  #定義した尤度関数で外から与えるデータ
      control = list(fnscale = -1,  #最大化問題の場合はfnscale = -1
                     maxit=1000)) #maxitは最大の計算回数

#講義で行った通り平均の最尤推定はサンプル平均
#また、講義ではできませんでしたが、分散の最尤推定の理論値はvar(x)
#標準偏差はsd(x)でできます
mean(x)
var(x)
sd(x)

#4. 正規分布の最尤推定
x<-c(0.92, 0.99, 1.01, 1.03, 1.07)


#5. 指数分布の最尤推定
x<-c(5.7, 7.1, 8.3, 10.6, 12.9)


#6.  負の二項分布
x<-c(121, 113, 125, 118, 119, 124)

#7. paired t test
before<-c(120, 118, 125, 131, 112, 108, 111, 121, 116, 109)
after<-c(118, 117, 118, 122, 109, 108, 109, 115, 112, 108)


#8. F検定
a<-c(121, 125, 119, 118, 126, 110, 120)
b<-c(123, 119, 121, 114, 127, 113, 118)