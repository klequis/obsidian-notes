# Haskell Projects

## Transforming data

Take a data set that needs to be transformed into a consistent and/or useful format and create an interface to query it. The interface can be just commands in a terminal or a terminal GUI.

A data set I did this for with NodeJS was checking and credit card transactions. Different accounts and banking institutions have significant variation in data format. Columns and column order varies, some have a single "amount" field, others have separate "credit" & "debit" fields. Sometimes a debit is a negative number, other times it is a positive number. What I made uses a small amount of metadata to transform all into a common format. As for querying, you have text, dates and numbers to work with.

## Haskell Web Server

There are a number of [web servers for Haskell](https://wiki.haskell.org/Web/Servers)

Take any data set, read it from file or database, and create an API. We could create a down and dirty JavaScript front-end to show it works.


One of my interests in the past was creating a privacy oriented personal financial management app. I'm not doing that anymore but there are a couple of interesting things we could do:

- parse bank & credit card info (in csv format) from different institutions and transform into a standard format that can be used by.
  - can build some terminal commands for it such as sum of debits or credits, transactions between dates, transactions that begin or contain "xyz", transactions >, < = x, all transactions for "ACME", etc.
  - parsing of any other data sets of interest?
- write a Haskell web server that can talk to an existing API/data provider of bank/financial data and a low-effort JavaScript frontend that proves it works. Two well know providers are https://plaid.com/ and https://www.mx.com/. 
  - Could do the same with a weather or other API (say weather) which would simplify or remove things like authentication.

Also
- markdown to html: Saw a couple of other haskell libs that were wrappers for a C library and I'll guess there is good cause to do so. We could write a pure Haskell implementation that doesn't need to be complete but whatever we can do in 2 to 3 weeks.

- Similar to above, could take any data set and write a Haskell API/Web server for it.
