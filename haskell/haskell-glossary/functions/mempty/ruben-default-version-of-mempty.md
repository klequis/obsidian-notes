> Ruben: It’s worth asking: what would it mean to have a default version of `mempty`? What would it be?

The best I can come-up with for that:

- I suspect it would be wrong 99.99% of the time
- I'm wondering if it would cause problems for writing Monoids
- Mean? I guess having a default means there is a way of doing something 
-   that is useful some significant percentage of the time. I'm not familiar
-   with very many Monoids yet to have a perspective on that but as said above
-   it wouldn't be useful/correct very often.