# javascript-studies

What it says.

# Helpers : a transparent syntax convention

###### ( 2025-05-08 )

Ah, IndexedDB, spaghetti API. Drafted one framework. Parked it. Found a
nicer idiom. Web is complicated so we should not make it more
complicated. More transparency, and pipey, and generally Unixy is a good
thing.

So for any standard web `object.method`, we can call a helper
function, like this : 

```javascript
a( object.method )
// this should do analysis, print an opinion, maybe save internal data
// to a, but neither modify nor run object.method

a( object.method )( arg1, arg2 )
// this can then behave like object.method(arg1, arg2), with sauce

a( object.method, options )( arg1, arg2 )
// this enables customised sauce, without messing up object.method's
// argument layout in the visible code
```
# Iteration Protocols

###### Class & Prototype Hierarchy 

-   class: `Iterator` : `.name` is 'Iterator' global scope, abstract superclass
    -   prototype: `AsyncIterator : `.[Symbol.asyncIterator]()` returns
        `this`
        - ... is the prototype of AsyncGenerator objects

-   `Iterable`, `AsyncIterable` : has two corresponding procotols
-   `GeneratorFunction`, `AsyncGeneratorFunction` : objects that return
    `Generator` and `AsyncGenerator` instances
-   `Generator`, `AsyncGenerator` : respective subclasses of `Iterator`,
    `AsyncIterator`

-   `function * (){}` creates an object

    -   `.prototype` is `Generator` / A 
        -   `.prototype` is `undefined` 
        -   `.constructor` is `GeneratorFunction` / B
            -   `.prototype` is `Generator` / D 
            -   `.constructor` is `GeneratorFunction` / C

    -   `.constructor` is `GeneratorFunction` / C
        -   `.constructor` is `Function` 
        -   `.prototype` is `GeneratorFunction` / B
            -   `.constructor` is `GeneratorFunction` / C
            -   `.prototype` is `Generator` / D

    -   `return`s a `Generator`
        -   `.prototype` is `undefined`, however 'Object.getPrototypeOf()` returns `Generator`
```
a = function * (){}


a.prototype                             // BB : Generator < Iterator < Function
a.prototype.prototype                   //    : undefined
a.prototype.constructor                 // AA : GeneratorFunction < Function 
a.prototype.constructor.prototype       // DD : Generator < Iterator < Function
a.prototype.constructor.constructor     // CC : GeneratorFunction < Function

a.constructor                           // CC 
a.constructor.constructor               // FF : Function < Object 
a.constructor.constructor.constructor   // FF 
a.constructor.constructor.prototype     //    : native function < Object, but not Function
a.constructor.prototype                 // AA 
a.constructor.prototype.constructor     // CC 
a.constructor.prototype.prototype       // DD 

Object.getPrototypeOf(a)                // AA
Object.getPrototypeOf(a.prototype)      // DD
Object.getPrototypeOf(a.constructor)    // FF

a : a GeneratorFunction object,

OK      - whose Object.getPrototypeOf() is AA : a GeneratorFunction object
OK          - whose superclass          is FF : the Function class

SUS     - whose prototype               is BB : a Generator object
OK          - whose superclass          is HH : the Iterator class

SUS     - whose constructor             is CC : a GeneratorFunction constructor ( simplest )

BB : a Generator object,

OK      - whose prototype               is DD : another Generator object

OK      - whose constructor             is AA : a GeneratorFunction constructor 
OK          - whose constructor         is CC : a GeneratorFunction constructor ( simplest )

b = a()

b.prototype                             // undefined

b.constructor                           // AA 

Object.getPrototypeOf(b)                // BB 

Theory : 

-   [ECMAScript Standard Built-inObjects](https://tc39.es/ecma262/multipage/ecmascript-standard-built-in-objects.html#sec-ecmascript-standard-built-in-objects) is a good place to start.

    Built-in **Function object** : a built-in object that is **callable** as a
    function, see
    [10.3.](https://tc39.es/ecma262/multipage/ordinary-and-exotic-objects-behaviours.html#sec-built-in-function-objects).

        Built-in **function objects** which are not built-in
        **constructors**, do not have **function object.[[Construct]]**
        but will have **function object.[[Call]]**, and do not have **
        function object.[[Prototype]]**, unless further specified.

    Built-in **Constructors** : are built-in **function objects** that are used
    with the `new` operator.

    Built-in **Prototype objects** : belong to built-in
    **constructors**; unless specified, each built-in
    **constructor.[[Prototype]]** = `Function.prototype`; unless
    specified, each built-in **prototype object.[[Prototype]]** =
    `Object.prototype`

-   `Object` is the `object constructor`; when called, as a
    **constructor** it creates a new ordinary object, otherwise it
    performs a type conversion; `object constructor.[[Prototype]]` =
    `Function.prototype`

        `Object.prototype` is an "immutable prototype exotic object",
        such that `Object.prototype` will not change after
        initialisation ; where "exotic objects" are defined as "not
        ordinary objects",and "ordinary objects" are defined in detail
        in the specification.

        Initially, `Object.prototype.constructor`=`Object`

-   The `Function constructor` is a built-in **function object**;
    `Function constructor.[[Prototype]]` = `Function.prototype`

    `Function.prototype` is a built-in **function object**; it does not
    have `Function.prototype.[[Construct]]` and therefore cannot be used
    with the `new` operator; `Function.prototype.[[Prototype]]` =
    `Object.prototype`.

-   The `Iterator constructor` is an abstract superclass; as it is
    defined to to be abstract, `new Iterator()` cannot create objects;
    only the subclasses of `Iterator` can.

    `Iterator constructor.[[Prototype]]` = `Function.prototype`

    `Iterator.prototype.[[Prototype]]` = `Object.prototype`

    `Iterator.prototype.constructor` returns `Iterator`.

-   The `GeneratorFunction constructor` = `GeneratorFunction` is a
    subclass of `Function`, inheriting from the `Function constructor`;
    creates and initialises a new `GeneratorFunction` when called as a
    function, not as a constructor; this is QUIRKY but idiomatic; it is
    a built-in **function object**; 

    `GeneratorFunction constructor.[[Prototype]]` = `Function`

    `GeneratorFunction.prototype` is NOT a **function object**;
    `GeneratorFunction.prototype.[[Prototype]]` = `Function.prototype`

    `GeneratorFunction.prototype.constructor` = `GeneratorFunction`

    `GeneratorFunction.prototype.prototype`

    RATHER UNCONVENTIONALLY, creating a `GeneratorFunction` instance
    creates TWO NEW OBJECTS, NOT ONE, because the
    `GeneratorFunction.prototype` is assigned a NEW object; FURTHERMORE,
    UNCONVENTIONALLY the new object assigned to
    `GeneratorFunction.prototype` is not an instance of
    `GeneratorFunction`, but an instance of `Generator`; this instance
    of `Generator` is then used to initialise the
    `Generator.[[Prototype]]` which is returned when the
    `GeneratorFunction` is called : 

-   `Generator` subclasses `Iterator`; `Generator` instances are
    constructed NOT by the `Generator` class, but by the
    `GeneratorFunction` class; WHY this is specified, remains a mystery;
    so, a `Generator` instance's `.prototype` is its creator
    `GeneratorFunction`'s `.prototype`; 

    CHECK TO SEE where
    `GeneratorFunction`s inherit from an entity called
    %GeneratorPrototype% === %Generatorfunction.prototype.prototype%,
    which is not a `Generator` instance; 
```

-   `async function * (){}` creates an object


### `Iterator` abstract superclass

Exists in the global scope. Has many instance methods.

### Iterable Protocol

The Itera**ble** Protocol defines **the Object containing data**, which other
functions can iterate over.

Definition :

>   Itera**ble** objects implement a method named `[Symbol.iterator]`.
>   -   arity : 0
>   -   this : the Itera**ble** object itself
>   -   returns : an Itera**tor**

###### Single-use/loop

`iterable[Symbol.iterator]()` conventionally returns `this` i.e.
`iterable`, on any invocation.

###### Multiple-use/loop

`iterable[Symbol.iterator]()` might return a new iterator object, on
each invocation.

### Iterator Protocol

The Itera**tor** Protocol defines **an object providing a
pointer/cursor**, to an Itera**ble** Object; this may or may not be the
Iter**able** Object itself.

Definition :

>   Itera**tor** objects MUST implement a method named `['next']`.
>   -   arity : 0 or 1 ( `yieldThis` )
>       -   NO built-in language features pass an argument here
>       -   any value input here, will be output by the corresponding
>         `yield()` expression, IF this Itera**tor** is a **generator**
>   -   this : unspecified
>   -   returns : an object implementing the `IteratorResult` interface
>
>       Definition of subsidiary : `IteratorResult` interface
>
>           IteratorResult object has properties
>           -   `done`, which will be tested as a `boolean`
>               -   **essential** : MUST evaluate to `true` if the calling
>                   `next()` failed to produce a value
>               -   **optional** : MUST evaluate to `false` if the calling
>                   `next()` was able to produce a value.
>           -   `value`, which contains any JavaScript value
>               -   **optional** : may or may not contain the value
>                   produced by `next()`, regardless of `done`
>
>   Iterat**or** objects may OPTIONALLY implement a method named
>   `['return']`.
>   -   **significance** : informs the Itera**tor** that there will be
>       no more calls to `next()`, so clean-up can commence
>   -   arity : 0 or 1 ( `value` )
>       -   NO built-in language features pass `value`
>   -   this : unspecified
>   -   returns : an object implementing the `IteratorResult` interface
>
>       `IteratorResult` implementation
>       -   conventionally, `done` is `true
>       -   conventionally, `value` is what was passed as
>           `return(value)`
>
>   Iterat**or** objects may OPTIONALLY implement a method named
>   `['throw']`.
>   -   **significance** : informs the Itera**tor** that an error has
>       been detected; typically this method will then `throw` the
>       `value` which was passed to `next()`
>   -   arity : 0 or 1 ( `exception` )
>       -   `exception` is typically an `Error` instance
>       -   NO built-in language features call `throw` during clean-up
>       -   `throw` is a heuristic for symmetry, when **generators** use
>           `return`
>   -   this : unspecified
>   -   returns : an object implementing the `IteratorResult` interface
>
>       `IteratorResult` implementation
>       -   conventionally, `done` is `true
>       -   `value` is unspecified

# `GeneratorFunction` class

Not a global object. Can be accessed syntactically by explicitly
creating a generator function, then referencing its constructor :
```javascript
new function*(){}.constructor
```
>   `GeneratorFunction` objects return : `Generator` objects,
>   where `Generator` is a subclass of `Iterator`


# `Promise`
###### ( 2025-05-(08,12) )

`async` just automates ( makes explit, into implicit ) 

1.  the wrapping of a 'normal' synchronous function in a `new Promise`
2.  the firing of the `resolve()` or `reject()` handlers, which were
    passed as arguments to : the executor ( 2-arity anonymous function ) 
    which is the first argument of `new Promise`.

```javascript
// this :
a = ( async _=> 0 )()

// ... is roughly, the same as this :
b = new Promise ( 
        (resolve, reject) => { try { resolve(0) } catch (error) { reject(error) } } 
        )
```
Because this is by definition what `await`/`async` does, there's no way
to extract the `resolve()` or `reject()` handles from an `async`
function.

### Promises : continuation passing style / CPS

And now, this is pretty cool ... as of 2024, the native syntax
simplifies usage of continuation passing style.

```javascript 
// like so
const { promise, resolveContinuation, rejectContinuation } = Promise.withResolvers()
```
Reduction of boilerplate :
```javascript
// like so
const continuations = {

    // this is all that needs doing
    concern1: Promise.withResolvers(),

    // anchor
    concern2: {}
}

// hoist : this is additional boilerplate avoided by (concern1)'s syntax
continuations.concern2.promise = new Promise((resolve, reject)=>{
    continuations.concern2.resolve = resolve
    continuations.concern2.reject = reject
    });

// code will jump to here 
(async _=> {
    await continuations.concern1.promise
    await continuations.concern2.promise
    console.log('all promises resolved')
})()

// code will jump from here 
continuations.concern1.resolve() 
continuations.concern2.resolve() 
```

###### ( 2025-05-06 )

JavaScript has a `Promise` class, representing technical debt. The general
form of which is :
```javascript
let p = new Promise( (resolve, reject) => condition ? resolve('shim') : reject('shim') )
await p
```
If neither the `resolve`-er nor `reject`-er are called, then execution will stall at `await p`.

It turns out that calling `resolve` looks sort of like [continuation
passing](https://en.wikipedia.org/wiki/Continuation), though I am not
(yet) sure that they are exactly the same. The following is illustrative
:
```javascript
/* 1. We extract the (continuation) from a Promise. */
let continuation 
const p = new Promise((resolve,reject)=>{
    continuation = resolve

    /* We can worry about reject() later */
});

/s* 2. Far, far away in our program, we can (await) the (continuation) : */
(async _=>{
    end(await p) /* end() is defined at 4. below, for illustrative order */
})();

/* 3. Somewhere else, and also far away, we can call the (continuation) : */
(_=>{
    /* ... deep and distant code complexity ... */
    continuation('treasure')
})();

/* 4. */
function end(printme){
    console.log(printme) // prints 'treasure'
};

```
# JavaScript Execution Workflow Algorithm 

*(2020-04 study; revised 2021-06-06)*

For the purpose of this note, we shall use the definition of *computation* as a
series of operations upon data.

This [zoomable map](https://www.plectica.com/maps/C7Z4HYSNU) by [Matt
Mamonov](https://medium.com/@g.smellyshovel/the-ecmascript-executable-code-and-execution-contexts-chapter-explained-fa6e098e230f)
is highly recommended for ease of use - though its accuracy is debatable.

## HOSTS 

Hosts may refer to Specifications or Runtimes.

Example: [WHATWG HTML is regarded as a Host Specification (4.2. and D) in the
ECMAScript
specification](https://tc39.es/ecma262/#sec-hosts-and-implementations).  It
should be noted that the HTML and ECMAScript specifications may contradict each
other, resulting in (certain browser behaviours of note), but that is not the
focus on this study. 

Example: a browser's JavaScript engine is a Host (implementation of the WHATWG
HTML specification).

### Executing Thread

Executing threads are implicitly defined.

Quote:

    NOTE 1 Some web browsers share a single executing thread across multiple
    unrelated tabs of a browser window, for example.

Implicitly then, an *executing thread* refers to an implementation-specific
logical thread of computation as defined by the ECMAScript Host's runtime.

## JOBS

ECMAScript refers to coded operations as
[Jobs](https://tc39.es/ecma262/#sec-Jobs) ... a type of [Abstract
Closure](https://tc39.es/ecma262/#sec-abstract-closure). Abstract Closures refer
to algorithms, so they are sequential. 

Quote:

    - Only one Job may be actively undergoing evaluation at any point in time.
    - Once evaluation of a Job starts, it must run to completion before
      evaluation of any other Job starts.

- Jobs sit in queues on the Host. 
- The Host gets to decide when Jobs are run, but the Host doesn't know how to
  run Jobs. So the Host passes control of the Execution Thread to the
  Surrounding Agent for any Job that gets to be executed.
- Each Job runs WITHOUT INTERRUPTION in browser implementation convention; the
  specification however, does ALLOW for an
[Agent](https://tc39.es/ecma262/#sec-agents) to block! 

    - Colloquially, "code execution is synchronous by default (by convention, not by specification)".

### Common Browser Convention: Separate Macro- and Micro-tasks Queues

- Browsers tend to have two Job queues, a HIGH-priority queue, and a LOW-priority
queue.
- Low-priority Jobs are from big-world events including user interface (UI)
events, such as button clicks and keypresses. 
- High-priority Jobs from
small-world events including coded ontologies, such as Promises. 
- When a
low-priority task completes, the browser will try to complete ALL outstanding
high-priority tasks, before proceeding to any outstanding low-priority task. 

    - As a result, Promises will, by convention, run before Timers.

```
TODO: Also, by convention (check?), each link in a Promise chain (e.g.
Promise(executor1).then(executor2) will 
```

#### ERRORS in PROMISES: 

During a Job, any errors thrown in Promises are queued as new Jobs, and will be
reported AFTER the CURRENT JOB, ... UNLESS they are CAUGHT, in which case you
can skip the queue and get your feedback IMMEDIATELY in the SAME JOB.

``` 
console.log ('begin') 
new Promise ( F => { throw Error } ) 
console.log ('
... in progress ... ') 
``` 

### Surrounding Agents & Agent Clusters

Surrounding [Agents (9.7.)](https://tc39.es/ecma262/#sec-agents) are appliances that manage
the Execution Contexts of any ECMAScript code gets to run on the thread. 

[Agent Clusters (9.8)](https://tc39.es/ecma262/#sec-agent-clusters) are Agents
that can share memory.

Different web browsers cluster their agents differently. The concept of an Agent Cluster
has already entered the HTML specification, and some server headers advise
clients on how to cluster Agents, notably by same-origin.

#### Execution Contexts & Execution Context Stacks

[Execution Contexts (9.4.)](https://tc39.es/ecma262/#sec-execution-contexts)
(ECs) bundle together the code, the code evaluation state, and the relevant
Realm.

[Execution Context Stacks](https://tc39.es/ecma262/#execution-context-stack) 
(ECS), have the global EC at the bottom.

Each Job creates an EC, so it is convenient (until further reading) to imagine a
1-to-1 mapping between Jobs and (EC)s. Each subroutine (sub-Job) recursively
inserts a new EC onto the ECS, and that EC is removed when the subroutine
returns. At any time, only ONE JOB can run, and therefore only one EC can be the
current **Running Execution Context**, per runtime Agent. Each subroutine that
is current will have pushed its parent ("calling") routine's EC under its own
EC, the ECS.

#### Realms

ECMAScript tracks state with [Realms (9.3.)](https://tc39.es/ecma262/#realm),
and as the name suggests, these determine the scope of addresses accessible by
executing code.

## Event Loop

It's not ECMAScript.  ECMAScript has no concept of an "event loop" - that is a
concept from WHATWG HTML (possibly appropriated from NodeJS?).

The colloquial "event loop" refers to one cycle which goes from an empty ECS to
an empty ECS. 

0.  The first operation in the loop, is to run whatever Job is next in queue
(whichever of the two queues has a Job, and is of the higher priority).  

1.  The
Job adds an EC to the ECS, and may subsequently call subroutines (sub-Jobs)
which add further ECs to the ECS. Again, subroutine ECs are removed when
subroutines return.  

2.  When all ECs have been cleared and the ECS is empty,
then the first Job is complete, and the next Job in queue will be run.

### Links of Note

- https://html.spec.whatwg.org/multipage/webappapis.html#task-source
- https://html.spec.whatwg.org/multipage/webappapis.html#integration-with-the-javascript-agent-formalism
