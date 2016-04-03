% Max Voit
% Shamir's Secret Sharing
% 05/05/2015 (first complete version)

# What is it?

Shamir's Secret Sharing [0] is an algorithm to split up a secret message into a
number of shares, n. The crux is that to recombine the shares to the secret
not all n but only k of them are required. Therefor (n - k) shares may be lost
and the secret still reconstructable. Also not only one bearer needs to be
compromised but at least k.

# Why?

The inspiration to implement this came from esgeh [1,2]. At the time I found it
to be a nice excercise that held various small challenges:

	- structuring data/types in a sensible way
	- parsing command line arguments
	- parsing and validating input (shares)
	- error handling
	- computation in finite fields (GF256)
	- checksum algorithm (CRC24)

# Resources

[0] http://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing
[1] https://github.com/sellibitze/secretshare
[2] https://github.com/sellibitze/secretshare-cxx
[3] http://www.zlib.net/crc_v3.txt
