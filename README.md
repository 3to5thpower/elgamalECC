# elgamalECC

[![Haskell](https://github.com/3to5thpower/elgamalECC/actions/workflows/haskell.yml/badge.svg)](https://github.com/3to5thpower/elgamalECC/actions/workflows/haskell.yml)

The Practice Implementation of EC Elgamal Encryption.

## Requirements

- [Haskell Stack(build tool)](https://docs.haskellstack.org/en/stable/README/)

## Usage

Clone this Repository and execute `stack run` to run the application.

```sh
$ stack run
# First, this app displays detailed information about keys and elliptic curves.
位数は99971です。
今回の秘密鍵は 9193, 公開鍵は(9757, 48359), 楕円曲線が y^2 = x^3 + 8254x + 3760, 生成元Gは(4108, 3619)です。
# Input any string
暗号化したい文字列を入力してください。
>>> hello
# Then, the encryption result and its decryption result are displayed.
暗号化結果は[((38725, 8015),(24936, 44767)),((38725, 8015),(77809, 58732)),((38725, 8015),(67936, 46681))]です。
復号化結果は"hello"です。
```

I defined Finite field at the type level so the order _p_ cannnot be changed at runtime.  
If you want to change the order _p_ from 99971, please rewrite the line 9 and 10 of `src/Data.hs`.
