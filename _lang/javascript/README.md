# javascript-studies

What it says.

# Promises
( 2025-05-06 study )
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
/* 1. We extrate the (continuation) from a Promise. */
let continuation 
const p = new Promise((resolve,reject)=>{
    continuation = resolve

    /* We can worry about reject() later */
});

/* 2. Far, far away in our program, we can (await) the (continuation) : */
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
