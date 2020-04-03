# Study Notes

These are based on [How Selections Work](https://bost.ocks.org/mike/selection/)
by Mike Bostock.

-   ## Selections are a *subclass* of `Array`.

-   ## Selections are 2D matrices (`Array`s of `Array`s)

    1.  The first dimension is called **GROUPS**
    2.  The second dimension contains **ELEMENTS**

-   ## Two API Patterns

    1.  The **D-S** pattern.

        -   `d3.select ( SELECTOR_STRING )` and
        -   `d3.selectAll ( SELECTOR_STRING )` 
        
        ... each of these API calls, returns selections with exactly one (1) **GROUP**.

        This group may have one (1) or many (N) **ELEMENTS**.

    2.  The **D-S-S** pattern.

        -   `d2.select    ( SELECTOR_STRING ).selectAll ( SELECTOR_STRING_OR_FUNCTION )` and
        -   `d2.selectAll ( SELECTOR_STRING ).selectAll ( SELECTOR_STRING_OR_FUNCTION )`

        ... the LATER API call for each of these call-chains, returns
        selections with exactly one (1) **GROUP** PER **ELEMENT** RETURNED BY
        the EARLIER corresponding API call.

-   ## Each **GROUP** has a `parentNode` property

    The `parentNode` property is set on the **GROUP** when the **GROUP** is
    created.

-   ## **SELECTOR_FUNCTION**s take three arguments

    1.  `d`, the current **ELEMENT**'s **DATUM** (typically stored as the
        `__data__` property of [the DOM node, or the DOM node's placeholder
        object]).
    2.  `i`, the current **ELEMENT**'s index position in its parent **GROUP**.
    3.  `nodes` (or `elements`, or `group`), the current **ELEMENT**'s parent
        **GROUP**.

    ... and `this` in the function, is bound to the current **ELEMENT** (or
    `group [ i ]`).
