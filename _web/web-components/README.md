
# A Systematic Introduction to "Web Components"
*Based on : the state of tech in **2024-05***
## Preamble
- ==IMPORTANT== : ( You may read  this **Preamble** and the **Executive Summary** first, but ) 
    The later parts of this text depend on an understanding of the
  earlier parts, so the parts should be read in order.
- ==IMPORTANT== : Contrary to common citation, there is NO OFFICIAL Web Components
  standard.
    - Web Components ( WebComponents.Org ) UNOFFICIALLY refers to the composite
      use of a set of technologies defined in other standards documents, mainly
        - the **Document Object Model ( DOM )** living standard, 
        - the **HyperText Markup Language ( HTML )** living standard, and 
        - the **Cascading Style Sheets ( CSS )** Scoping Module Level 1 W3C First
          Public Working Draft ( "work in progress, editor's draft, not an
          official standard" )
- HTML documents are interpreted within the context of the DOM.
    - The DOM specification exists in the context of **object oriented programming**
  and uses such language.
    - DOM *objects* ( ==henceforth, "objects"== ) have *interfaces* which define their
  usability. 
    - DOM objects often implement multiple interfaces at the same time. 

### Conventions in this Text 
- Objects are referred to by a shorthand convention below, where "a
       `Something`" refers to an object which implements the `Something` interface …
       and "`Somethings`" refers to all objects which implement the `Something`
       interface.
- "DOM" refers **explicitly** to the DOM ( the model ), and **not implicitly**
  to *node trees* as is commonly practiced elsewhere.
- Effort has been made to put *terms in italics* when they are terminology that
  has been used in specifications - however there may been exceptions due to
  error.
- Effort has been made to put `terms in monospace` when they are terminology
  that appears in programming code - however there may been exceptions due to
  error.

## 0. Executive Summary

### 0.1. *Custom `Elements`* : 

#### by themselves,
- … utilise,
    - … `CustomElementRegistry`,
    - … `CustomStateSet`, and
    - … `CustomEvent`, to
- … allow augmented `Elements` which may have customised 
    - … behaviour, including user-interaction behaviour, 
    - … event emission and event reaction,
    - … appearance, including CSS selection targets, while
- … facilitating code reuse for the above ; and

#### … may optionally have, 
- … improved code reusability, via `<template> Elements`, 
- … improved operational security, via `ShadowRoots`, and moreover
- …improved code reusability vis-a-vis composability, via `<slot> Elements` ( i.e.
  *custom elements* can be composed from other *custom elements* ).

### 0.2. The `<template> Element`, by itself :
- … fundamentally, addresses HTML's concern for semantic tagging of content, by
  designating a unique `Element` for the purpose of templating ;
- … trivially, contains content meant for reuse ( you could just clone any node
  tree, or copy any markup ).

### 0.3. The `ShadowRoot` interface : 

#### … by itself, 
- … provides encapsulation for styling, and
- … provides encapsulation for programmatic node traversal ;

#### … is functionally extensible via optional `<slot> Elements`,
- … which only work as descendants of a `ShadowRoot`,
    - … taking some `Slottables` from outside a *shadow tree*, and 
    - … rendering them inside the *shadow tree*, thereby
- …. facilitating "composition" design patterns, for markup which can then be
  distributed between a *node tree*, and its *shadow-including subtrees* ; and

#### … optionally extends the functionality of `<template> Elements`, 
- … whereby somewhat non-trivially, a `<template> Element` may be configured to
  replace itself with a `ShadowRoot`, in a *node tree*, 
    - … by extending the `<template> Element`'s `.content` property's
      `DocumentFragment` into a `ShadowRoot`,
    - … attaching that `ShadowRoot` to [ the `<template> Element`'s parent
      `Element` ]
      as its *shadow host*, and
    - … then removing the `<template> Element` from the *node tree*, which 
    - … in essence, saves a few manual programming steps, without doing anything unique.

## 1. `Nodes`
- `Node` is a fundamental DOM interface. 

### 1.1. `Elements`
- `Element` is also an important DOM interface.
- `Nodes` may be `Elements` ( a.k.a. HTML elements, further defined under that
  specification ).
- Important `Nodes` which are not `Elements` include : 
    - `Texts`, 
    - `Documents`, 
    - `DocumentFragments`, 
    - `DocumentTypes`, 
    - `Attrs`, 
    - `Comments`, and more.
```
rough drawing : some, not all, DOM interfaces
~~~~~~~~~~~~~~~~~~
- Interface EventTarget 
  +- extended by Interface AbortSignal
  +- extended by Interface Node 
         +- extended by Interface Attr
         +- extended by Interface CharacterData 
         |      +- extended by Interface Comment
         |      +- extended by Interface ProcessingInstruction
         |      +- extended by Interface Text
         |              +- extended by Interface CDATASection
         +- extended by Interface Document
         +- extended by Interface DocumentType
         +- extended by Interface DocumentFragment
         +- extended by Interface Element
```
    
## 2. Parents, Collections, & Trees
-   This section talks about the arrows / pointers / directed-edges between
    Nodes.

### 2.1. *`Node` Trees*
-   `Nodes` are organised as "node trees" ( roughly : acyclic- directed- graphs ).
    -   `Nodes` do not form any organisation which contradicts the concept of a
        tree - for example,
        -   you may have a list of `Nodes`, or a collection of *node trees*, but
        -   you may not have a cyclic graph of `Nodes`, in the DOM.
-   `Node` trees may be *document trees*, *shadow trees*, or neither of the two.
-   "Rendering" refers to a user agent's operation upon some *node trees*, which
    outputs a graphical interpretation of those *node trees*.

### 2.2. Single Parentage
-   `Nodes` have `.parentNode` and `.parentElement` properties.
    -   A `Node`'s `.parentNode` points to their sole parent in their *node tree*.
        Thus `Nodes` may belong to only one *node tree* at a time.
    -   All `Elements` are `Nodes`, but not all `Nodes` are `Elements`, so [ for any
        object, O, O.parentElement must be null, when O.parentNode is not an
        `Element` ].
-   The inverse pointers are
    -   the `.childNodes` property, that typically refers to a `NodeList`, and
    -   the `.children` property, that typically refers to a `HTMLCollection`.

### 2.3. "Collections"
-   `NodeLists` and `HTMLCollections` are informally referred to as "collections".
    -   `NodeList` is an interface for a collection of `Nodes`.
    -   `HTMLCollection` is an interface for a collection of `Elements`.
-   Collections do not typically implement the `Node` interface.

### 2.4. Illustrations
#### 2.4.1.
```
rough drawing : a node tree, in a common web browser ( not all nodes are shown )
~~~~~~~~~~~~~~~~~~
Object#0
implements Document
prop, documentElement   -> #1           Object#1
prop, childNodes        [ -> #1 ]---+   implements Element
prop, childElements     [ -> #1 ]   +---prop, parentNode    -> #0
prop, parentNode null                   prop, tagName       "html"
                                +-------prop, childNodes    [ -> #2 ]
                           +----+       prop, ownerDocument -> #0
                           |            prop, childElements [ -> #2]
                           |
Object#2                   |
implements Element         |           
prop, parentNode    -> #1--+
prop, tagName       "body"             
prop, childElements [ -> #4 ]          
prop, ownerDocument -> #0
prop, children      [ -> #5 ]                   
prop, childNodes    [ -> #3, -> #4, -> #5 ]---------+
                                                    |
                                   +----------------+-------+
Object#3                           |                |       |
implements Text                    |                |       |
prop, parentNode            -> #2--+                |       |
prop, data                  "First bit of text."    |       |
prop, nextSibling           -> #4                   |       |
prop, nextElementSibling    -> #5                   |       |
prop, ownerDocument         -> #0                   |       |
                                                    |       |
Object#4                           +----------------+       |
implements Text                    |                        |
prop, parentNode            -> #2--+                        |
prop, data                  "Second bit of text."           |
prop, nextSibling           -> #5                           |
prop, nextElementSibling    -> #5                           |
prop, previousSibling       -> #3                           |
prop, ownerDocument         -> #0                           |
                                                            |
Object#5                                                    |                       
implements Element                                          |
prop, parentNode            -> #2---------------------------+
prop, tagName               "div"
prop, previousSibling       -> #4
prop, ownerDocument         -> #0
```    
#### 2.4.2. DOM *node tree* Notation : a missing specification? 
- For any **markup**, the DOM may rearrange the *node tree* differently, including  automatic removal and insertion of `Elements`  that were not in the original markup. 
- For any for any ***node tree***, the DOM may render things differently based on differentiated rules.
- So, for any new feature introduced to the environment, one has demonstrate { the **markup**, the ***node tree***, and the **rendering** }. Otherwise there are unapparent corner cases.

Short-hand notation for DOM *node trees* ... it doesn't exist. If you open your browser developer tools and look at the rendered *node tree* it is actually in an **unofficial, non-specified format** decided by the browser vendor.

Which is how you get :shit: like :
```
<div class="shadow-host">
    #shadow-root
        <div>This text will be rendered.</div>
        <slot name="jack">
            THIS TEXT WILL NOT BE RENDERED.
            ↳<span> 🎯🖱reveal : Text that is not here in
                                 the *node tree*, but which 
                                 will be rendered here.
        </slot>
        <div>This will be rendered.</div>
    THIS TEXT WILL NOT BE RENDERED. ( Sibling of *shadow root*. )
    <span slot="jack>
        This this will be rendered, but it does not render
        at the location of this location in the *node tree*;
        instead it is rendered inside the &lt;slot&gt; element.
    </span>
</div>
<div class="sibling-of-shadow-host">
   This text will be rendered.
</div>
```

## 3. *Root Nodes*
-   Certain interfaces must be at the *root* of their respective *node trees*.
    -   `Documents`, `DocumentFragments`, and `ShadowRoots` must have their
        `.parentNode` and `.parentElement` properties set to null ( "they may not
        have parents" ).
 -   Various other `Nodes` may be *roots*, but the following are of special
     interest.
    
### 3.1. Documents
-   `Nodes` may be `Documents` ( a.k.a. *document roots* a.k.a. *documents* ).
    -   `Documents` typically belong to a user agent, for example in most web
        browsers the `window` variable name refers to the user agent object, and
        the `window.document` property name refers to the window's *document
        root*.
    -   ==WARNING== : the `window.document.documentElement` property name refers to
        [ the *document*'s HTML `Element`, which is the child of the window's
        *document root* ] - it does not refer to the *document root* itself.
-   A *document tree* refers to a *node tree* whose *root* is a `Document`.
    -   `Nodes` have an `.ownerDocument` property, which points to their one and
        only ancestor `Document`.
    -   A `Document`'s `.ownerDocument` must be null.
-   **RENDERING** : A user agent typically renders any changes to its document
    trees, as soon as possible.
    
### 3.2. DocumentFragments
-   `Nodes` may be `DocumentFragments`.
    -   `DocumentFragments` have no special properties.
    -   For clarity, `DocumentFragments` are *root* `Nodes` which are neither *document
        roots* nor *shadow roots*.
    -  > ( The DOM spec neither recommends, nor recommends against, the use of
        the term "document fragment root" which would be isomorphic with the
        spec-recommended terms *document root* and *shadow root*. )
    -   > ( The DOM spec neither recommends, nor recommends against, the use of the
    term "document fragment tree" which would be isomorphic with the
    spec-recommended terms *document tree* and *shadow tree*. )
-   A new `DocumentFragment`, DF, constructed in a browser window's realm, via
    `new DocumentFragment` will typically be constructed with
    `DF.ownerDocument===window.document`.
    -   Whereas, if you create a new `Document`, D2, and use
        `D2.createDocumentFragment` to construct a new `DocumentFragment`, DF2, then
        `DF2.ownerDocument===D2`.
-   **RENDERING** : Unlike with *document trees*, a user agent does not render any
    *node trees* whose *roots* are `DocumentFragments`.
    -   So, `DocumentFragments` may be used merely as parents for their
        `.children` and `.nodeList`, to collect *node trees* which are not
        immediately rendered by the user agent.
    -   An exception to this is, when a `DocumentFragment` is a `ShadowRoot`.

#### 3.2.1. `ShadowRoot` & "Shadow Host" Pairs
-   `DocumentFragments` may also be `ShadowRoots` ( a.k.a. *shadow roots* a.k.a.
    *shadows* ).
-   `ShadowRoots` each have a `.host` property, which must point to an `Element`,
    called the `ShadowRoot`'s *shadow host*.
    -   A `ShadowRoot`'s `.host` is never `null` - `ShadowRoots` may not be created or
        manipulated without having a *shadow host*.
    -   The inverse pointer is the *shadow host* `Element`'s `.shadowRoot` property. So,
        -   if and only if, a `ShadowRoot`, S,
            -   has a property, `S.host`,
            -   which points to an `Element`, E,
        -   then, `E.shadowRoot` points to S. 
            - ( Caveat : unless S was created with `{ mode : "closed" }` )
-   There are only two ways to create a `ShadowRoot` and *shadow host* pair.
    -   **Declaratively** via a `<template>` `Element`'s `shadowrootmode` attribute.
        -   This attribute is discussed further in another section of this text,
            **Templates and their `.shadowrootmode` Attribute**.
    -   **Imperatively** via an `Element`'s `.attachShadow` method.
        -  `.attachShadow`accepts the following key parameters in an object :
            -   **Required**
                -   `mode` : `"open"` | `"closed"`
                    -   `"open"` exposes the `ShadowRoot` via the *shadow host*'s
                    `.shadowRoot` property
                    -   `"closed"` nullifies this property ; however, the `ShadowRoot`
                    is still returned when it is constructed via the *shadow
                    host*'s`.attachShadow`method
            -   **Optional**
                -   `clonable` : `true` | `false` ( default value )
                -   `delegatesFocus` : `true` | `false` ( default value )
                -   `slotAssignment`  : `"manual"` | `"named"` ( default value )

-   A *shadow tree* refers to a *node tree* whose *root* is a `ShadowRoot`.
    -   ==WARNING== : 
        - An [ `Element`'s *shadow root* ] refers to [ the `ShadowRoot`
          descendant of that `Element` ], and not to [ the `Element`'s *root*
          which is a `ShadowRoot` ].
            - An [ `Element`'s *shadow tree* ] refers to [ the *node tree* of
              the`ShadowRoot` descendant of that `Element` ], and not to [ the
              `Element`'s own *node tree* even if it is also a *shadow tree* ].
            - [ the *node tree* of a `ShadowRoot` ] is not [ the *node tree* of
              that `ShadowRoot`'s *shadow host* ], thus
            - frustratingly,  an [ `Element`'s *root* which
        is a *shadow root* ], is not the value of [ that `Element`'s `.shadowRoot`
        property ].
        -   From the perspective of [ the `ShadowRoot` and its *shadow tree* ], [ the
            *node tree* of the `ShadowRoot`'s *shadow host* ] may be referred to as a *light
            tree*.
            -   That *light tree* may itself be a *shadow tree* … if its root is another
            `ShadowRoot`.
    -  > ( The DOM spec neither recommends, nor recommends against, the use of
        the term "light DOM". This term apparently evolved naturally from the
        term "shadow DOM", and is informally used synonymously with the
        DOM-spec-recommended term *light tree*. )
    -   > ( The DOM spec neither recommends, nor recommends against, the use of
        the term "host tree", where it has preferred instead to recommend the
        term *light tree*. )
    -   > ( The DOM spec neither recommends, nor recommends against, the use of
        the term "shadow DOM". This term is recommended by WebComponents.Org, to
        be used synonymously with the DOM-spec-recommended term *shadow tree*. 
        This term also occurs in draft specifications which have been
        deprecated. )
     -   Below, an entire section of this text is devoted to **Ordinary *Roots*
         and *Shadow-including Roots***.
-   `ShadowRoots` each have a `.slotAssignment` property, which determines how
    their descendant `<slot> Elements` are rendered.
    -   Without configuration ( by default ), this property is set to `"named"`,
        but it may also be set to `"manual"`. The effect of both settings is
        discussed below in this text, under the section **`slot Elements` &
        `Slottables`**.
    -   The `.slotAssignment` property cannot be changed after the `ShadowRoot`'s
        construction.
-   ***Node tree* traversal** : `Node` methods such as `.querySelector` do not traverse
    from an `Element`'s *node tree*, to the `Element`'s *shadow tree*.
    - To    traverse a `shadow tree`, one has to call the respective traversal method on
    a `Node` ( such as the `ShadowRoot` ) in the *shadow tree*.
-   **Styling** : Style declarations affecting a *node tree*, NT, do not select
    `Elements` in [ *shadow trees* whose *shadow hosts* are in NT ]. There
    are two ways to style *shadow trees*.
    
    -   **Imperative** styling : `ShadowRoots` have an `.adoptedStyleSheets`
        property which houses an array of `CSSStyleSheets`.
        -   This array should be : modified "in-place", and not "replaced".
        -   `CSSStyleSheets` should be created with the
                `document.CSSStyleSheet` method.
        -   Each `CSSStyleSheet` is treated as a style sheet that has been
               **inserted after other stylesheets in the *shadow tree***.
        -   Each `CSSStyleSheet` may concurrently belong to the
                `.adoptedStyleSheets` of multiple `ShadowRoots`, and also to the
                `.adoptedStyleSheets` of their collective *document root*, such that
                changing the `CSSStyleSheet` will affect all adopted parents.
    
    - **Declarative** styling : `<style> Elements` can be written into a *shadow tree*,
      which will thus be scoped to affect only that *shadow tree*.
        
        -   `<style> Elements` may be injected into a *shadow tree* via `<template> Elements`.
        
    -   **CSS selectors of interest** :
        -   `:host` pseudo-class
        -   `:host()` pseudo-class
        -   `:host-context()` pseudo-class
       
-   **RENDERING** : Unlike other *node trees* whose *roots* are `DocumentFragments`,
    *shadow trees* are **rendered as soon as possible**.
    -   ==WARNING== : when an `Element` becomes a *shadow host*, its children
        are no longer rendered by user agents
        - it is as if the children
        have the CSS property `display : none`
        -  … however an examination of the
        child `Elements` will show that both, their initial *node tree* location (
        as descendants of the *shadow host* ), and initial value of the CSS property `display`,
        have not changed ( the latter may be `block`, `inline`, etc. ).
        -   The DOM, HTML, and CSS specifications do not contain a description
            of this behaviour, which is implemented for all *shadow hosts*.  (
            If this is specified, I have not yet found the specification. )
    -   Furthermore, a *shadow host*'s *shadow root*'s
        `.children` will be rendered in place of the *shadow host*'s children.
    -   Furthermore, there is a special behaviour for `<slot> Elements` which are
        children of *shadow roots* - this is discussed in another section of this
        text, **`<slot> `Elements` & `Slottables`**.
    -   >( The DOM spec neither recommends, nor recommends against, the use of
        the term "shadow boundary". This term is recommended by
        Developer.Mozilla.Org, for rendered elements, separating the [ rendered
        `Elements` in a *shadow tree* ], from the [ rendered `Elements` their
        *light tree* ].  )
    
#### 3.2.2.  Ordinary *Roots* vs. *Shadow-including Roots*

-   `Nodes` have a `.getRootNode` method.
    
-   By default ( when called with no arguments ), or when called with the
    argument `{ composed: false }`, a `Node`'s `.getRootNode` method returns the
    *root* `Node` of the `Node`'s *node tree*.
    
    -   For a node, N, `N.getRootNode( { composed : false } )` returns the result of
        following `N.parentNode`, recursively,

        -   until the first instance of an ancestor node, R, is encountered,
            where `R.parentNode=null`.
        
    -   >SPECIFICATION : If an [ object's parent ] is null, the [ object's root ]
        is [ itself ]; otherwise, the [ object's root ], is the [ object's
        parent's root ].
        
    -   >( The DOM spec neither recommends, nor recommends against, the use of
        the terms "shadow-excluding roots", "uncomposed root", "simple roots",
        or "ordinary roots" to be used in opposition to the spec-recommended
        term *shadow-including root*. )
        
-   When called with the argument `{ composed : true }`, a `Node`'s `.getRootNode`
    method returns the *shadow-including root* node of the `Node`'s *node tree*.
    
    -   For a node, N, `N.getRootNode( { composed : true } )` results from
        following `N.parentNode`, recursively,
    
        -   whereas as each instance of an ancestor node, SR, is encountered,
            where `SR.parentNode=null` and SR is a `ShadowRoot`, instead check
            `SR.host.parentNode`,
    
        -   until the first instance of an ancestor node, R, is encountered,
            where `R.parentNode=null` and R is not a `ShadowRoot`.
    
    -   >SPECIFICATION : If an [ object's root ] is a shadow root, the [ object's
        "shadow-including root" ] is the [ object's root's host's
        shadow-including root ]; otherwise, the [ object's shadow-including root
        ] is the [ object's root ].
    
    -  > ( The DOM spec neither recommends, nor recommends against, the use of
        the term "composed root" to be used synonymously with the
        spec-recommended term *shadow-including root*. )   

#### 3.2.3. Discussion of Technology Names related to `ShadowRoot` and *Shadow Hosts*

-   ==WARNING== : The morphological choice of "shadow" to refer to this technology
    demands annotation. The DOM relationship between *light trees* and *shadow
    trees*, is not comparable to the relationship between physical objects and
    their physical shadows.
    
-   **Usable analogy** : A *shadow tree* is hosted by some other specific `Element`, and
    thus follows along wherever that `Element` goes.
    
    -   Apart from this, *shadow trees* do not have much in common with ordinary
        shadows whose name they borrow.
    
-   **Poor analogy** : A *shadow tree* is not necessarily isomorphic with its *light
    tree*, and in most cases they are not isomorphic.
    
    -   In fact, *shadow trees* can be built pretty much independently from their
        *light trees*, without collision.
    
-   **Poor analogy** : *Shadow trees* generally do not exhibit fewer phenomena or
    fewer dimensions than *light trees*.
    
    -   In fact, *shadow trees* have almost all the appearance and behavioural
        properties of *light trees*, with few caveats ; a *node tree* can be
        both a *light tree* and a *shadow tree* at the same time.
    
-   **Compounded poor analogy** : see note on *light tree*.
    
    -   Where is t.f. light?
    
-   **Compounded poor analogy** : there exists an `Element`, the `<template> Element`,
    whose `.innerHTML` markup is not rendered by the user agent, but instead parsed
    into a unrendered `DocumentFragment`, and stored in the `<template> Element`'s
    `.content` property ... as if it were hidden in shadows.
    
    -   `<template> Elements` do have a configuration which replaces them with
        `ShadowRoots`, but this is off by default, and anyway, most *shadow
        hosts* are not `<template> Elements`.
    
-   **FUNCTIONALITY** : the main feature of *shadow trees*, is that they are SPATIALLY
    ORTHOGONAL ( as in "namespaced", "memory spaced", and "hyper- or
    other-dimensional" ) versus their *shadow hosts*' *node trees* ( and might
    thus be better called "guest trees" or "protected trees" ).
    
    -   This hyperdimensionality allows *shadow trees* to
        optionally maintain SPATIAL segregation ( encapsulation ) from their
        *light trees*, for node traversing operations such as : event
        propagation, CSS scoping, and element selection via JavaScript API.
    
        -   Thus *shadow roots* behave similarly to `<iframe> Elements`.
    
        -   In comparison with `<iframe> Elements`, *shadow roots* consume less
            resources, and have poorer isolation of every kind, but have tighter
            couplings with their hosts.
    
    -   The *shadow root* security model is such that,
        
        -   *shadow trees* are slightly more
            protected from scripts in their *light trees*, whereas
            
            -   A *shadow host* ( in the *light tree* ) accesses their *shadow
                tree* via the *shadow host*'s `.shadowRoot` property,
                alternatively via the return value of the *shadow
                host*'s`.attachShadow`method.
        -   *light trees* are slightly less protected from
            scripts in their *shadow trees*.
            
            -   A `ShadowRoot` ( in the *shadow tree* ) accesses their *light
                tree* via the `ShadowRoot`'s `.host` property.
            
### 3.3. Illustrations
```
rough drawing : 

    the numbering of *nodes* here is arbitrary,
    and does not follow any specified algorithm;

    algorithms that only traverse *node trees* by following 
    ".childNodes" and ".parentNode" pointers will never 
    traverse into, or out of, *shadow trees* ;

    - NODE#0 is a *document root*
    - NODE#6, NODE#10 are *shadow roots*
    - NODE#4, NODE#8 are their *shadow hosts*
    
    each *shadow root* is the root of a *shadow tree*, and
    each *document root* is the root of a *document tree*;
    the DOM spec has it that the *shadow trees* are *associated*
    with the *document tree*, but they are not considered
    part of the *document tree* - this phrasing is somewhat unfortunate;
    however, given that the spec further contains a concept of 
    *shadow-including roots*, one might venture to say that the
    sum of the *document tree* and its *associated shadow trees*
    may be called a *shadow-including document tree*; -_- ;

    - NODE#0 is { NODE#0, #1, #2, #3, #4, #5 }'s *root*, which is not a *shadow root*
    - NODE#6 is { NODE#6, #7, #8, #9 }'s *root*, which is a *shadow root*
    - NODE#10 is { NODE#10, #11 }'s *root*, which is a *shadow root*

    - NODE#0 is the *shadow-including root* of { NODE#0 ... #11 }

~~~~~~~~~~~~~~~~~~
- NODE#0 : Interface : Document 
  ^
  |- pointers : .childNodes x .parentNode
  v
  NODE#1 : Interface : Element : <html>
  ^
  |- pointers : .childNodes x .parentNode
  v
  NODE#2 : Interface : Element : <body>
  ^  ^
  |  |- pointers : .childNodes x .parentNode
  |  v
  |  NODE#4 : Interface : Element : <div class="a-shadow-host">
  |  ^
  |  :\\\
  |  : \\\- EXTRAORDINARY pointers : .shadowRoot x .host
  |  :  \\\
  |  :   \\\ NODE#6 : Interface : ShadowRoot
  |  :       ^  ^
  |  :       |  |- pointers : .childNodes x .parentNode
  |  :       |  v
  |  :       |  NODE#8 : Interface : Element : <div class="a-shadow-host">
  |  :       |  ^ 
  |  :       |  :\\\ 
  |  :       |  : \\\- EXTRAORDINARY pointers : .shadowRoot x .host 
  |  :       |  :  \\\ 
  |  :       |  :   \\\ NODE#10 : Interface : ShadowRoot 
  |  :       |  :       ^ 
  |  :       |  :       |- pointers : .childNodes x .parentNode 
  |  :       |  :       v
  |  :       |  :       NODE#11 : Interface : Element :  
  |  :       |  :           <div class="child-of-a-shadow-root">
  |  :       |  :               This would be rendered, but WITHOUT the influence 
  |  :       |  :               of styles declared upstream of NODE#10.
  |  :       |  :           </div>
  |  :       |  :       
  |  :       |  :       
  |  :       |  :- pointers : .childNodes x .parentNode
  |  :       |  v
  |  :       |  NODE#9 : Interface : Element : 
  |  :       |      <div class="child-of-a-shadow-host">
  |  :       |          This would be rendered, but WITHOUT the influence
  |  :       |          of styles declared upstream of NODE#6,
  |  :       |          UNTIL NODE#8 becomes a shadow host.
  |  :       |          After that, it would not be rendered.
  |  :       |      </div>
  |  :       |
  |  :       |
  |  :       |- pointers : .childNodes x .parentNode
  |  :       v
  |  :       NODE#7 : Interface : Element : 
  |  :          <div class="child-of-a-shadow-root">
  |  :              This would be rendered, but WITHOUT the influence
  |  :              of styles declared upstream of NODE#6.
  |  :          </div>
  |  :
  |  :
  |  :- pointers : .childNodes x .parentNode
  |  v
  |  NODE#5 : Interface : Element : 
  |     <div class="child-of-a-shadow-host">
  |         This would be rendered, UNTIL NODE#4 becomes a shadow host.
  |         After that, it would not be rendered.
  |     </div>
  |
  |
  |- pointers : .childNodes x .parentNode
  v
  NODE#3 : Interface : Element : 
      <div class="not-a-shadow-host not-child-of-a-shadow-host">
          This would rendered normally.
      </div>
```
 
## 4. `<template> Elements` and their `.shadowrootmode` Attribute

-   `<template> Elements` have unique behaviours, different from most other
    elements.
    
    -   Of note, there are six kinds of HTML `Elements`, and the `<template>
        Element` is the sole-member of its entire kind
        
    -   ( The other five kinds for your further reading are,
        

        -   void elements ( no end tag required, such as `<br>` ),
            
        -   raw text elements ( `<script>`, and `<style>` ),
            
        -   escapable raw text elements ( `<textarea>`, `<title>` ),
            
        -   foreign elements ( MathML, SVG namespaces ), and
            
        -   normal elements. )
    

-  > HTML spec excerpts    
    
    > "The template element is used to declare fragments of HTML that can be
        cloned and inserted in the document by script."
     
     > "In a rendering, the template element represents nothing."
    
     > "The template element can have template contents, but such template
        contents are not children of the template element itself. Instead, they
        are stored in a `DocumentFragment` associated with a different `Document` —
        without a browsing context — so as to avoid the template contents
        interfering with the main Document." 
     > -   This is located at the `Element`'s `.content` property.
        
-   `<template> Elements` may have a `shadowrootmode` content attribute.

    -   Without configuration ( by default ), `<template> Elements` do not have a
        *shadow root*.
        
    -   If …
       
        -   a `<template> Element`'s `shadowrootmode` content attribute is
            declared in the start tag, with the value `"open"` or `"closed"`, and
            
        -   the `<template> Element` is [ the first `<template> Element` child of
            its parent, with a `shadowrootmode` content attribute with the value
            `"open"` or `"closed"` ]

    -   … then …
            
        -   1. the [ `<template> Element` ‘s parent `Element`'s `.shadowRoot` property
            ] is set to a `ShadowRoot`, whose
            
            -   1.1. `.mode` property is set to the declared value
                
            -   1.2. `.nodeList` property is set to the value of [ the `<template>
                Element`'s `.content` property ]
                
            -   1.3. `.clonable` property is set to `"true"` ( since `<template>
                Elements` are meant to be cloned for reuse ) . And
            

        -   2. The `<template> Element` is itself removed from its *node tree* (
            "disappears" ).
```
rough drawing : 
~~~~~~~~~~~~~~~~~~
1.	Example, <template> without `shadowrootmode` attribute :

	If, in your markup, you write :
	
		<div id="not-planning-to-become-a-shadow-host">
			Hello there, I'm `Text` object A.
			<template>
				Hello there, I'm `Text` object B.
			</template>
		</div>

	... then, if you use JavaScript to examine the *node tree*, you will find that :

		- NODE#0 : Interface : Element : <div> 
		  ^  ^
		  |  |- pointers : .attributes x .ownerElement
		  |  v
		  |  NODE#1 : Interface : Attr : id : "not-planning-to-become-a-shadow-host"
		  |  
		  |  
		  |- pointers : .childNodes x .parentNode  
		  |  
		  +-> NODE#2 : Interface : Text : "Hello there, I'm `Text` object A."
		  |
		  +-> NODE#3 : Interface : Element : <template>
			  |
			  |- pointers : .content
			  v
			  NODE#4 : Interface : DocumentFragment ( is a *root* )
			  ^
			  |- pointers : .childNodes x .parentNode
			  v
			  NODE#5 : Interface : Text : "Hello there, I'm `Text` object B."

	... and, what will be rendered is :

		Hello there, I'm `Text` object A.

2.	Example, <template> with `shadowrootmode` attribute :
	
		<div id="eventually-becomes-a-shadow-host">
			Hello there, I'm `Text` object A.
			<template shadowrootmode="open">
				Hello there, I'm `Text` object B.
			</template>
		</div>

	... then, if you use JavaScript to examine the *node tree*, you will find that :

		- NODE#0 : Interface : Element : <div> 
		  ^  ^  ^
		  |  |  |- pointers : .attributes x .ownerElement
		  |  |  v
		  |  |  NODE#1 : Interface : Attr : id : "eventually-becomes-a-shadow-host"
		  |  |
		  |  |
		  |  |- pointers : .childNodes x .parentNode
		  |  |
		  |  v		  
		  |  NODE#2 : Interface : Text : "Hello there, I'm `Text` object A."
		  |  	( This will not be rendered. )
		  |
		  |- pointers : .shadowRoot x .host  
		  v
		  NODE#3 : Interface : ShadowRoot ( is a *root* )
		  |		( This has replaced the NODE : Interface : Element : <template> )
		  |
		  |- pointers : .childNodes x .parentNode
		  v
		  NODE#4 : Interface : Text : "Hello there, I'm `Text` object B."

	... and, what will be rendered is :

		Hello there, I'm `Text` object B.
```
        

## 5. `<slot> Elements` & `Slottables`

-   Despite the fact that `<template> Elements` and `<slot> Elements` are frequently
    discussed together in documentation, `<slot> Elements` do not behave
    differently based on their location in relation to `<template> Elements`.
    
    -   `<slot> Elements` descended from `<template> Elements` behave just like any
        other `Elements` descended from `<template> Elements`.
        
    -   `<slot> Elements` behave in their own unique way, regardless of whether
        they do, or do not, have a `<template> Element` ancestor.
        
    -   The special behaviours of `<slot> Elements` and `<template> Elements` can
        interpolate, without contradiction. ( ==This requires some checking.== )
            
-   **CSS Selectors of interest** :
        -   `::slotted` pseudo-element
    
-   **RENDERING** :
    
    -   `<slot> Elements` are normally rendered with the CSS property `display :
        contents` ( the `<slot> Element` is replaced by its various contents, in
        the box-tree); this is the case, whether the `<slot> Element`'s
        `.assignedNodes` method returns an empty list, or some `Nodes`.
        
    -   If a `<slot> Element`'s `.assignedNodes` method returns the empty list, then
        the `<slot> Element`'s rendered contents are its `.childElements`.
        
    -   If a `<slot> Element`'s `.assignedNodes` method returns a non-empty list of
        `Nodes` ( its *assigned Nodes* ), then the `<slot> Element`'s contents are
        the value returned by the `<slot> Element`'s `.assignedNodes` method ( and,
        the `<slot> Element`'s `.childElements` will be ignored for rendering ).

        -   Also see : **Shadow host > RENDERING > WARNING**

### 5.1. Qualification of `Slottables` for *Assignment* to `<slot> Elements`
        
-   `<slot> Elements` have unique behaviours, different from most other elements.
    ( Of the six types of HTML elements, they belong to the "normal elements". )
    
    -   They may or may not have a `name` attribute - which optionally allows
        them to be targeted for the *assignment* of `Slottables`.    

        -   Elucidated below, in this text : `Slottables` may be *assigned* to a
            ***default slot*** that has no `name` attribute.

    -   They have an `.assign` method, which allows manual *assignment* of
        `Slottables` to the `<slot> Element`.
        
        -   The `.assign` method only functions when a `<slot> Element`'s *root* is a
            [ *shadow root* whose `.slotAssignment` property is set to `"manual"` ].
        
    -   They have an `.assignedNodes` method, which returns a list of *assigned
        `Slottables`* ; `Slottables` have an `.assignedSlot` property, which is the
        inverse pointer.
        
        -   The relationship is, **one-`<slot>-Element`-to-many-`Slottables`**.
            
        -   The *assignment* of `<slot> Elements` and `Slottables` to each other, is
            different based on the following qualifications.
            
        -   ==WARNING== : *Assignment* of `Slottables` to `<slot> Elements`,

            -  Does not change the initial location of `Slottables` in their *node
                tree*.
                
            -   Merely renders `Slottables` "as if" they had been moved from their
                initial location, to new locations as children of their
                `.assignedSlot`.
                
            -   See : RENDERING
    

-   **Qualified Interfaces** : `Slottables` must have DOM interfaces that are
    qualified for *assignment* to `<slot> Elements`.
    
    -   `Elements` and `Texts` are `Slottables`.
        
    -   >DOM spec : "Note : a slot can be a slottable."
        
-   **Qualified Root** : Broadly, a `<slot> Element`'s `.assign` method behaves
    differently, depending on whether its *root* is, or is not, a *shadow root*.

    -   If a `<slot> Element`'s *root* is not a *shadow root*, then the `<slot>
        Element`'s `.assign` method will never find a `Slottable` with a
        qualifying location, so the `<slot> Element`'s `.assignedNodes` method
        will always return an empty list.   

-   **Qualified Location** : More specifically, `Slottables` must be in a
    qualifying location, in order to be assigned to a `<slot> Element`. Only [
    immediate child `Slottables` ] of the [ `<slot> Element`'s [[ *root* that is
    a *shadow root* ]]'s *shadow host* ] may qualify.  ==TO DO : write about
    shadow trees, whose shadow hosts are in an upstream shadow tree==
    
    -   Attempted assignment of `Slottables` in disqualified locations will be
        ignored. Elucidatory examples of ignored `Slottables` :
    
        -   `Slottable` descendants of [ the `<slot> Element`'s [[ *root* that
            is a *shadow root* ]] ].
            
        -   `Slottable` [ siblings or grandchildren ] of [ the `<slot>
            Element`'s [[ *root that is a shadow root ]]'s *shadow host* ].     

    -   If a `<slot> Element` (SLOT)'s *root* is [ a *shadow root*, with a
        `.slotAssignment` property set to `"named"` ] (ROOT_S_N), then,
        

        -   ( **CASE1** ) … whereas, if SLOT has a `name` attribute, then [ the
            `name` attribute of SLOT ] will be matched to [ the `slot` attribute
            of one-or-more `Slottables` in qualified locations ], and the
            matched `Slottables` will be *assigned* to SLOT.
            
        -   ( **CASE2** ) … whereas, If SLOT has no `name` attribute, then …
    
            -   ( **CASE2A** ) … whereas, if SLOT is [ the first `<slot> Element` (
                without a `name` attribute ) descendant of ROOT_S_N ] … then SLOT
                is designated the ***default slot***, and
                
                -   … all `Slottables` in qualified locations, which have no
                    `slot` attribute, will be *assigned* to the ***default slot***.

            -   ( **CASE2B** ) … whereas, if SLOT is not [ the first `<slot> Element`
                ( without a `name` attribute ) descendant of ROOT_S_N] … then
                SLOT will be ignored for the *assignment* of `Slottables`, and
                SLOT's `.assignedNodes` method will always return the empty list.

    -   If a `<slot> Element`'s *root* is [ a *shadow root`, with a `.slotAssignment`
        property set to `"manual"` ], then ( **CASE3** ),
        
        -   The `<slot> Element`'s `name` attribute will be ignored, in the
            process of matching `Slottables` to this `<slot> Element`.
            
            -   Likewise, any respective `slot` attributes of `Slottables` in
                qualifying locations will have no effect.
            
        -   The initial behaviour of the `<slot> Element`, is as if its *root* was
            not a *shadow root*.
            
        -   The `<slot> Element`'s `.assign` method may be used to manually *assign* (
            one or more ) qualified `Slottables` to the `<slot> Element`, in a
            single manual *assignment* operation.
            
            -   A `<slot> Element`'s `Slottable` *assignments* can be reset to the
                empty list, by a manual  *assignment* operation with zero qualified
                `Slottables`.
        
    -   **Unassignable `Slottables`** in Qualified Locations, as implied by exclusion
        from **CASES 1, 2, and 3** :
        
        -   one or more `<slot> Elements` share [ a *root* ( which is a *shadow root*
            ) that has a `.slotAssignment` property set to `"named"` ], but
            
            -   ( exclusion from **CASE1** ) … whereas, the `Slottable` in a qualified
                location has a `slot` attribute whose value does not match any
                `<slot> Element`'s `name` attribute; or
                
            -   ( exclusion from **CASE2** ) … whereas, the `Slottable` in a qualified
                location has no `slot` attribute, and there is no designated
                ***default slot***; or          

        -   ( excluded from ***CASE3*** ) the `<slot>` `Element`'s *root* ( which is a
            *shadow root* ) has a `.slotAssignment` property set to `"manual"`, but
            the `<slot> Element`'s `.assign` method was not called on any `Slottables`
            in qualified locations.

### 5.1. Illustrations

#### 5.1.1. `<slot> Element` without `ShadowRoot` Ancestor
```
rough drawing : 
~~~~~~~~~~~~~~~~~~
Example, ( expect nothing to happen ) :

	If, in your markup, you write :
	
		<div id="not-planning-to-become-a-shadow-host">
			Hello there, I'm `Text` object A.
			<span id="a-slottable-element">
				Hello there, I'm `Text` object B.
			</span>
			<slot>
				Hello there, I'm `Text` object C; also a `Slottable` ;
				in fact, I'm the default content of a &lt;slot&gt;.
			</slot>
		</div>

	... then, if you use JavaScript to examine the *node tree*, you will find that :
	
		- NODE#0 : Interface : Element : <div> 
		  ^  ^
		  |  |- pointers : .attributes x .ownerElement
		  |  v
		  |  NODE#1 : Interface : Attr : id : "not-planning-to-become-a-shadow-host"
		  |  
		  |  
		  |- pointers : .childNodes x .parentNode  
		  |  
		  +-> NODE#2 : Interface : Text : "Hello there, I'm `Text` object A."
		  |
		  +-> NODE#3 : Interface : Element : <span>
		  |	  ^
		  |	  |- pointers : .childNodes x .parentNode
		  |	  v
		  |	  NODE#4 : Interface : Text : "Hello there, I'm `Text` object B."
		  |
		  +-> NODE#5 : Interface : Element : <slot>
		  	  ^
		  	  |- pointers : .childNodes x .parentNode
		  	  v
		  	  NODE#6 : Interface : Text : 
			  	  "Hello there, I'm `Text` object C; also a `Slottable` ; 
				  in fact, I'm the default content of a &lt;slot&gt;."
				  
	... and, what will be rendered ( with fewer line breaks ) is :
	
		Hello there, I'm `Text` object A.
		Hello there, I'm `Text` object B.
		Hello there, I'm `Text` object C; also a `Slottable` ;
		in fact, I'm the default content of a <slot>;.

	... beacuse, there were no Slottables in qualified locations, so 
		the <slot> simply rendered with its default content.
```
#### 5.1.2. `<slot> Element` with `ShadowRoot` Ancestor
- `<slot> Elements`, descended from
	- a `ShadowRoot` whose `.slotAssignment` has defaulted to `"named"` ;
- some `Slottables` in a qualified locations ; 
- some `Slottables` in disqualified locations.

```		
rough drawing : 
~~~~~~~~~~~~~~~~~~

Example : 

	If, via any method, you create a *node tree* (informally illustrated,
	using current browser developer tool conventions) :
	
		<div id="a-shadow-host">
			#shadow-root ( config : `.slotAssignment`="named" )
				<slot name="descendant-of-the-shadow-root-with-name">
					A : This will be rendered, UNTIL it is replaced with
						any `Slottable` in a qualified location, with an attribute
						`slot="descendant-of-the-shadow-root-with-name"`	
				</slot>
				<slot id="first-slot-descendant-of-the-shadow-root-with-no-name-becomes-default-slot">
					B : This will be rendered, UNTIL it is replaced with
						any `Slottable` in a qualified location, with no
						`slot` attribute. == CHECK : undefined, or empty string ? ==	
				</slot>
			((	C : First `Slottable` child of the *shadow host*. This will be 
					rendered after it is *slotted/assigned* to the *default slot*. 
			))
			<span id="second-slottable-child-of-the-shadow-host">
				D : This will be rendered after it is *slotted/assigned* to 
					the *default slot*. 
					`span[id="second-slottable-child-of-the-shadow-host"]` is
					in a qualified location.
			</span>
			<span id="third-slottable-child-of-the-shadow-host" 
					slot="descendant-of-the-shadow-root-with-name">
				E : This will be rendered after it is *slotted/assigned* to 
					`slot[name="descendant-of-the-shadow-root-with-name"]`. 
			</span>
			<span id="fourth-slottable-child-of-the-shadow-host"
					slot="unmatchable-slot-name">
				<span id="grandchild-of-the-shadow-host" 
						slot="descendant-of-the-shadow-root-with-name">
					F : THIS WILL NOT BE RENDERED, because 
						`span[slot="unmatchable-slot-name"]` is unmatched,
						whereas `span[id="grandchild-of-the-shadow-host"]` 
						is not in a qualified location.  
				</span>
			</span>
		</div>
		<span id="sibling-of-the-shadow-host" 
				slot="descendant-of-the-shadow-root-with-name">
			G : THIS WILL NOT BE RENDERED, because 
				`span[id="sibling-of-the-shadow-host"]`	is not in a
				qualified location.  
		</span>
		
	... and, what will be rendered ( with fewer line breaks ) is :
			
				E : This will be rendered after it is *slotted/assigned* to 
					`slot[name="descendant-of-the-shadow-root-with-name"]`. 
					
				((	C : First `Slottable` child of the *shadow host*. This will be 
					rendered after it is *slotted/assigned* to the *default slot*. 
				))
				
				D : This will be rendered after it is *slotted/assigned* to 
					the *default slot*. 
					`span[id="second-slottable-child-of-the-shadow-host"]` is
					in a qualified location.
				
```

## 6. CustomElementRegistry & CustomStateSet

-   These DOM interfaces implement the the HTML spec's concept of *custom
    `Elements`*.
    
-   `CustomElementRegistry` is an interface that does what it says. The user agent
    can provide an object with this interface typically via
    `window.customElements`.
    
-   Existing documentation on `CustomElementRegistry` & `CustomStateSet` appears to
    be straightforward, and thus sufficient.

    -   **Customised built-in `Elements`** : are extensions of built-in `Elements`.
        
    -   **Autonomous custom `Elements`** : are not extensions of built-in `Elements`.

-   **CSS Selectors of interest** :   
    -   `::state()` pseudo-class


# Recommendations for Building Web Components
==INCOMPLETE WORK IS IN PROGRESS, THROUGHOUT THE SECTIONS BELOW==
Since the standards and frameworks are hardly mature, is there a safe approach in the present, with a minimal surface area?
In other words, is there a framework for this? ( There are lots of frameworks for this ... -_- )

## 1. Broad Questions

### 1.1. Building Extensible Web Pages
- If you're building a web page, how might you markup some data such that ...
	- (a) the web page functions fully on its own
	- (b) someone could come along later and "throw in" a web component that merely ingests (a) and spits out a better web page?

#### **DISCUSSION :**
- If (b) is to involve ShadowRoots ( and it might not ), then a minimal convention when building (a) might involve **enforcing structures** which make it possible to 
	- (i) create a *shadow host*,
	- (ii) stick some `<slot> Elements` under (i)'s `ShadowRoot`
	- (iii) have important content from (a) flow into (ii) with minimal effort

##### Design Pattern
- A somewhat defensive approach, without assuming that a `shadow tree`-less `Document` will later have *shadow trees*, but being prepared for a contingency in which someone might want to convert the `documentElement` ( usually a `<html> Element` ) or some of its descendants into *shadow hosts*, might be to ...
	- ensure that each major **layout** section of a `Document` is in an immediate child of the `documenElement`, such that someone who wanted to move any major section into a `shadow tree`, can do that simply by ...
	- i. converting `documentElement` into a *shadow host*, which would hide all descendants
	- ii. setting up a *default slot* in `documentElement`'s *shadow tree*, such that all the *shadow host*'s hidden descendants are rendered as `Slottables` insider the *shadow tree*
	- iii. proceed to manipulate the `Slottables` from within the `shadow tree` via 
		- JavaScript : `Slottable .assignedNodes` 
		- CSS : == check and confirm "what the obvious methods are" == 
### 1.2. Building Usable Web Components
- If you're building a web component, how might you structure it such that ...
	- (a) the web component functions in a useful way, or in a nearly useful way, on its own
	- (b) someone could "throw in" your web component into their web page, and use it effectively, with minimum effort?

#### **DISCUSSION :**
- If (b) is to involve ShadowRoots ( and it might not ), then a minimal convention when building (a) might involve **enforcing structures** which make it possible to 
	- (i) stick some `<slot> Elements` under the `ShadowRoot`
	- (i) only ingest data from *light trees* to *shadow trees* via `Slottables`

##### Design Pattern
- When writing a web component for an unknown userbase, one should
	- specify a *default slot*
	- specify how `Slottables` in the component's *shadow host* should be arranged : either targeting *named slots* or the *default slot*

### 1.3. Building Web Component Frameworks
- What are some **complexities** which might evolve from the underlying technologies?
- Given those complexities, what is the best way to abstractly **describe classes of new problems**?
- Given abstract problem statements, what are some solution **design patterns** which might be cogent responses?

#### 1.3.1. Inter-tree Communication
- do we use `Events` to pass messages?
- can we stick a **document message broker** in the *shadow-including document root* a.k.a. `Node.ownerDocument` of the *node tree*?
	- can we stick **subtree message brokers** at intermediate `ShadowRoots` which have other `ShadowRoots` associated with their descendants?


## A. Appendix of Web Component Frameworks
-
 
## B. Distributed Documents
- Is it too early to imaging that various branches of a *document tree* might be implemented by different DOM user agents, and networked together? See : ** Inter-tree Communication**
- 
