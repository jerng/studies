# Study Notes

These are based on [How Selections Work](https://bost.ocks.org/mike/selection/)
by Mike Bostock.

-   ## Selections are a *subclass* of `Array`.

-   ## Selections are 2D matrices (`Array`s of `Array`s).

    1.  The first dimension is called **GROUPS**
    2.  The second dimension contains **ELEMENTS**

-   ## Two API Patterns

    1.  The **D-S** pattern

        -   `d3.select ( SELECTOR_STRING )` and
        -   `d3.selectAll ( SELECTOR_STRING )` 
        
        ... each of these API calls, returns selections with exactly one (1) **GROUP**.

        This group may have one (1) or many (N) **ELEMENTS**.

    2.  The **D-S-S** pattern

        -   `d3.select    ( SELECTOR_STRING ).select    ( SELECTOR_STRING_OR_FUNCTION )`,
        -   `d3.selectAll ( SELECTOR_STRING ).select    ( SELECTOR_STRING_OR_FUNCTION )`,
        -   `d3.select    ( SELECTOR_STRING ).selectAll ( SELECTOR_STRING_OR_FUNCTION )` and 
        -   `d3.selectAll ( SELECTOR_STRING ).selectAll ( SELECTOR_STRING_OR_FUNCTION )`

        ... the LATER API call for each of these call-chains, returns
        selections with exactly one (1) **GROUP** PER **ELEMENT** RETURNED BY
        the EARLIER corresponding API call.

-   ## Each **GROUP** has a `parentNode` property.

    The `parentNode` property is set on the **GROUP** when the **GROUP** is
    created.

-   ## **SELECTOR_FUNCTION**s take three (3) arguments.

    1.  `d`, the current **ELEMENT**'s **DATUM** (typically stored as the
        `__data__` property of [the DOM node, or the DOM node's placeholder
        object]).
    2.  `i`, the current **ELEMENT**'s index position in its parent **GROUP**.
    3.  `nodes` (or `elements`, or `group`), the current **ELEMENT**'s parent
        **GROUP**.

    ... whereas `this` in the function, is bound to the current **ELEMENT** (or
    `group [ i ]`).

-   ## Significant differences between `.select ()` and `.selectAll ()`:

    1.  `.select ()`'s second dimension, **ELEMENTS** *always* contains only one
        (1) **ELEMENTi** per **GROUP**.

        1.  `d3.select ( SELECTOR_STRING )` *always* returns only one (1)
            **GROUP** and that group *always* contains only one (1) **ELEMENT**.
        2.  `d3.select    ( SELECTOR_STRING ).select    (
            SELECTOR_STRING_OR_FUNCTION )` *always* returns only one (1)
            **GROUP** and that group *always* contains only one (1) **ELEMENT**.
        3.  `d3.selectAll ( SELECTOR_STRING ).select    (
            SELECTOR_STRING_OR_FUNCTION )` will return many (N) **GROUP**s in a
            1-to-1 correspondance with the (N) **PARENT ELEMENT**s returned by the
            first API call to `d3.selectAll ( SELECTOR_STRING)`.
            
            -   Each of these **GROUP**s will have exactly one (1) **ELEMENT**.
            -   Each of these **ELEMENT**s will receive the **DATUM** propagated
                from its **PARENT ELEMENT**.

        Groupings from the parent selection are **preserved**. 

        >   "data-joins are not needed here"

        The hypothesis above needs to be evaluated thoroughly for complete
        understanding.

        >   `.append` and `.insert` are wrappers on top of `select`, so they
        >   also preserve grouping and propagate data

        "In these cases, child elements will inherit parent element data." -
        presumably by a pointer reference.

    2.  `.selectAll ()`'s second dimension, **ELEMENTS** will contain many (N)
        **ELEMENT**s per **GROUP**.
    
        1.  `d3.select ( SELECTOR_STRING) .selectAll (
            SELECTOR_STRING_OR_FUNCTION )`'s first dimension, **GROUPS** will contain
            many (N) **GROUP**s in a 1-to-1 correspondance with the (N) **PARENT
            ELEMENT**s returned by the first API call to `d3.select (
            SELECTOR_STRING )`.
        2.  `d3.selectAll ( SELECTOR_STRING) .selectAll (
            SELECTOR_STRING_OR_FUNCTION )`'s first dimension, **GROUPS** will contain
            many (N) **GROUP**s in a 1-to-1 correspondance with the (N) **PARENT
            ELEMENT**s returned by the first API call to `d3.selectAll (
            SELECTOR_STRING )`.

        Groupings from the parent selection are **NOT preserved**.

        >   "data-joins are needed here"

        The hypothesis above needs to be evaluated thoroughly for complete
        understanding.

-   ## Data is bound to DOM elements, hence persistent.

    Whereas, selections are considered ephemeral.       

    >   Data is bound to elements one of several ways:
    >   
    >   -   Joined to groups of elements via `selection.data`.
    >   -   Assigned to individual elements via `selection.datum`.
    >   -   Inherited from a parent via `append`, `insert`, or `select`.

    Data is stored in the `__data__` property of each DOM node.

-   ## `selection.data` is A **HUGE EXCEPTION**

    >   -   `selection.data` can accept either a constant value or a function.
    >
    >       ...
    >
    >   -   **selection.data defines data per-group** rather than per-element: data
    >       is expressed as an array of values for the group, or a function that
    >       returns such an array. Thus, **a grouped selection has correspondingly
    >       grouped data!**

    -   ###  **PAIRING KEYS** are used to dereference **GROUP**ed data, for assignment to **group**ed elements.

        This is "data-joining".

        1.  Grouped data can be assigned to grouped elements by **__index__**, this
            is the default. This obviously works only when the data and elements are
            already respectively grouped in the same order.

        2.  Alternatively, a **KEY FUNCTION** can be used, which takes a **DATUM**
            as input, and returns a **KEY**. When mapping a [group of new data], to a
            [ [group of elements] already bound to a [group of old data] ], the **KEY
            FUNCTION** is run on **__each and every, old and new__** datum from the
            old and new groups, respectively.

            -   CAVEAT: when [joining many (N) groups of elements already bound to
                old data, with many (N) groups of new data], it is the case that
                group-pairs are processed in sequence, and [each group of elements is
                joined with its corresponding group of new data] such that keys are
                unique only within each [pair of [element-old-data]-[new-data]
                groups].

    -   ### Terminology for data-join outcomes

        >   -   Update - There was a matching element for a given datum.
        >       -   returned by `selection.data()`
        >           -   sorted in the order of the **NEW** data
        >
        >   -   Enter - There was no matching element for a given datum.
        >       -   returned by `selection.enter()`
        >           -   retained in the order of the **NEW** data
        >
        >   -   Exit - There was no matching datum for a given element.
        >       -   returned by `selection.exit()`
        >           -   sorted in the order of the **OLD** data/elements

        The so-called **ENTER SELECTION** is special,  

        >   because it represents elements that do not yet exist
        >
        >   An enter selection contains placeholders rather than DOM elements; these
        >   placeholders are simply objects with a __data__ property. The
        >   implementation of enter.select is then specialized such that nodes are
        >   inserted into the group’s parent, replacing the placeholder. 
        >
        >   This is why it is critical to call selection.selectAll prior to a data
        >   join: it establishes the parent node for entering elements.


