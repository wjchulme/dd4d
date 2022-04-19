
set.seed(22)

# with known data set ----


N <- 1000
testdata <- data.frame(
  a = sample(c("a","b","c"), size=N, replace=TRUE),
  b = sample(c("x","y","z"), size=N, replace=TRUE),
  c= rnorm(N),
  d= rnorm(N),
  date = as.Date(runif(N,0,99999), origin="1970-01-01")
)

known_variables <- c(
  "a", "b", "c", "d", "date"
)

sim_list = list(
  x1 = bn_node(~rcat(n=..n, c("a","b","c"), c(0.5,0.4,0.1))),
  x2 = bn_node(~as.integer(runif(n=..n, c, c+120))),
  x3 = bn_node(
    ~as.integer(runif(n=..n, c, c+120)),
    missing_rate = ~0.5
  ),
  x4 = bn_node(
    ~as.integer(runif(n=..n, d, d+120)),
    missing_rate = ~ (1-(x1=="a")*(1-0.1)),
    #needs = "a"
  )
)


bn <- bn_create(sim_list, known_variables = known_variables)

bn_plot(bn)
bn_plot(bn, connected_only=TRUE)

dummydata <-bn_simulate(bn, known_df = testdata, keep_all = TRUE, .id="patient_id")


# without known data set ----


sim_list = list(
  x1 = bn_node(~rcat(n=..n, c("a","b","c"), c(0.5,0.4,0.1))),
  x2 = bn_node(~as.integer(runif(n=..n, , 120))),
  x3 = bn_node(
    ~as.integer(runif(n=..n, (x1=="b")*10, 10 + (x1=="b")*10)),
    missing_rate = ~0.3
  ),
  x4 = bn_node(
    ~as.integer(runif(n=..n, 0, 120)),
    missing_rate = ~ (1-(x1=="a")*(1-0.1)),
    #needs = "a"
  ),
  x5 = bn_node(
    ~rnorm(n=..n, mean=x2, sd=0 + (x1=="a")*10),
    missing_rate = ~(x1=="c"),
    needs="x3"
  )
)


bn <- bn_create(sim_list, known_variables = known_variables)

bn_plot(bn)
bn_plot(bn, connected_only=TRUE)

dummydata <-bn_simulate(bn, pop_size=100, keep_all = TRUE, .id="patient_id")


