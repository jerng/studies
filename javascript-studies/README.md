# javascript-studies

What it says.

## 2020-04 Studies.

For the purpose of this note, we shall use the definition of COMPUTATION as a
series of operations upon data.

###### OPERATIONS: 

The ECMAScript TASK management MODEL refers to coded operations as Job Abstract
Closures (jobs). Jobs are modelled as a **FIFO** *QUEUE*. 

###### DATA: 

The ECMAScript MEMORY management MODEL refers to scopes (address access) as
Execution Contexts (ECs). Each "runtime / realm" Agent consists of a **LIFO** EC
*STACK* (ECS), with the global EC at the bottom, a Job Queue, etc.  (Realm has a
technical meaning, this is not it.) 

Each job creates an EC, so it is convenient (until further reading) to imagine a
1-to-1 mapping between jobs and (EC)s. Each subroutine (sub-job) recursively
inserts a new EC onto the ECS, and that EC is removed when the subroutine
returns. At any time, only ONE JOB can run, and therefore only one EC can be
the current **Running Execution Context**, per runtime Agent. Each subroutine that 
is current will have pushed its parent ("calling") routine's EC under its own EC, 
the ECS.

###### ERRORS in PROMISES: 

During a job, any errors thrown in Promises are queued as new jobs, and will be
reported AFTER the CURRENT JOB, ... UNLESS they are CAUGHT, in which case you
can skip the queue and get your feedback IMMEDIATELY in the SAME JOB.

```
console.log ('begin')
new Promise ( F => { throw Error } )
console.log (' ... in progress ... ')
```
###### Aside, on BROWSER implementation CONVENTION:

Browsers tend to have two job queues, a HIGH-priority queue, and a
LOW-priority queue. Low-priority jobs are from big-world events
including user interface (UI) events, such as button clicks and keypresses.
High-priority jobs from small-world events including coded ontologies, such as
Promises. When a low-priority task completes, the browser will try to complete ALL
outstanding high-priority tasks, before proceeding to any outstanding
low-priority task. As a result, Promises will, by convention, run before Timers.
Also, by convention (check?), each link in a Promise chain (e.g.
Promise(executor1).then(executor2) will 

Each job runs WITHOUT INTERRUPTION in browser implementation convention; the
specification however, does ALLOW for an agent to block! Colloquially, "code
execution is synchronous by defaulti (convention)".

### "Event Loop"

The colloquail 'event loop' refers to one cycle which goes from an empty ECS to
an empty ECS. 

0.  The first operation in the loop, is to run whatever job is next in
    queue (whichever of the two queues has a job, and is of the higher priority).
1.  The job adds an EC to the ECS, and may subsequently call subroutines (sub-jobs)
    which add further ECs to the ECS. Again, subroutine ECs are removed when
    subroutines return.
2.  When all ECs have been cleared and the ECS is empty, then the first job is 
    complete, and the next job in queue will be run.

