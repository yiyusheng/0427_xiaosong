z <- exp(-seq(0,3, by=0.1)^2 )
plot(z)
plot(diff(z))
z[ which(abs(diff(z))==max(abs(diff(z))) )]
# [1] 0.6126264
# could have also tested for min() instead of max(abs())
plot(z)
abline( v = which(abs(diff(z))==max(abs(diff(z))) ) )
abline( h = z[which(abs(diff(z))==max(abs(diff(z))) ) ] )
