library(rsconnect)


rsconnect::setAccountInfo(name='phacochr',
                          token='5EF4F3866483046C4E2C16F98ED6FA29',
                          secret='flZSH863SQkPv0qq1Qj5sVH4FfdjcWTXHiMXKNGc')

rsconnect::deployApp('/home/user/github/phacochr_data')
