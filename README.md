# Anvil
A safe package manager in Idris

# Plans:

* Change Packages so that they don't store dependencies
  + Dep -> Pkg function, used for comparisons to installed packages/adding to list
    - How can we do constraint -> version without a db?
* Create meta-package objects? (These come from the DB, but are _NOT_ the same as the package type)
* S-expression based packaging format?
* Static config through type provider (!!!)
