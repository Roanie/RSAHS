# RSA

This is a simple RSA cipher to demonstrate how a computer could encrypt and decrypt with RSA.
This is for a school presentation and the code is not ment to be very good;
it's something that was hacked together quickly and while at the same time learning a bit of haskell.

All of the code is in the App folder and the code has two dependencies
  - [System.Random](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
  - [Text.Printf](https://hackage.haskell.org/package/base-4.14.0.0/docs/Text-Printf.html)



The main function which is called, while in the ghci interactive enviroment, via `:main` is
sort a step-by-step working through the creation of all the variables needed to perform RSA and then encrypting and decrypting 
the users input.

Key pairs can be generated via the code: `(n, e, d) <- keyPair`

Or alternativly an encryption and decryption function can be
generated via the code: `(encrypt, decrypt) <- rsaFuncs`
