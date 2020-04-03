# Study Notes

These are based on [How Selections Work](https://bost.ocks.org/mike/selection/)
by Mike Bostock.

-   ## Selections are a *subclass* of `Array`.

-   ## Selections are 2D matrices (`Array`s of `Array`s)

    1.  The first dimension is called **GROUPS**
    2.  The second dimension contains **ELEMENTS**

-   ## Two API Patterns

    1.  The **D-S** pattern.

        -   `d3.select ( ... )` and
        -   `d3.selectAll ( ... )` 
        
        ... each of these API calls, returns selections with exactly one (1) **GROUP**.

        This group may have one (1) or many (N) **ELEMENTS**.

    2.  The **D-S-S** pattern.

        -   `d2.select    ( ... ).selectAll ( ... )` and
        -   `d2.selectAll ( ... ).selectAll ( ... )`

        ... the SECOND API call for each of these call-chains, returns
        selections with exactly one (1) **GROUP** PER **ELEMENT** RETURNED BY
        the FIRST corresponding API call.
