This module contains some pseudo-definitions of internally defined types.
These definitions can be translated using the `kics2c` binary to obtain
their Haskell representation, which can then be used as the template
for their implementation in the runtime.

For instance, the module `Nat` contains the internal implementation
of narrowable `Int` numbers in Curry, which is included in the runtime
as well as in the external implementations of the module `Prelude`.
After generation, the Haskell representation is then adjusted by removing
the `C_`-prefix of data types and alike.

The old script `mkPrimTypes.sh` essentially tries to automate this
using a bunch of `sed` calls, but this script is outdated and therefore
may not work anymore.
