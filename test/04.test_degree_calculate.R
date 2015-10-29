
degree.add(30, 60)     # 90
degree.add(60, 30)     # 90
degree.add(200, 300)   # 140
degree.add(300, 200)   # 140
degree.minus(50, 20)   # 30
degree.minus(20, 50)   # 330
degree.minus(30, 330)  # 60
degree.minus(330, 30)  # 300


degree.seq(from = 10, to = 180, length.out = 20)
degree.seq(from = 180, to = 10, length.out = 20)
degree.seq(from = 180, to = 10, length.out = 20, restrict = TRUE)

degree.seq(from = 180, to = 10, length.out = 20)
degree.seq(from = 180, to = 10, length.out = 20, restrict = TRUE)