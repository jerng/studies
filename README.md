# javascript-studies

What it says.

## 2020-04-08 Revision: 

For the purpose of this note, we shall use the definition of COMPUTATION as a
series of operations upon data.

###### OPERATIONS: 

The ECMAScript TASK management MODEL refers to coded operations as Job Abstract
Closures (jobs). Jobs are modelled as a QUEUE. Each job runs WITHOUT
INTERRUPTION - colloquially, "the running thread blocks / code execution is
synchronous by default". 

###### DATA: 

The ECMAScript MEMORY management MODEL refers to scopes (address access) as
Execution Contexts (ECs). Each REALM "runtime" consists of an EC Stack (ECS),
with the global EC at the bottom. 

Each job creates an EC, so it is convenient (until further reading) to imagine a
1-to-1 mapping between jobs and (EC)s. Each subroutine (subjob) recursively
inserts a new EC onto the ECS, and that EC is removed when the subroutine
returns. At any time, only ONE JOB can run, and therefore only one EC can be
CURRENT, per runtime REALM. Each subroutine that is current will have pushed its
parent ("calling") routine's EC under its own EC, in the ECS.

###### ERRORS in PROMISES: 

During a job, any errors thrown in Promises are queued as new jobs, and will be
reported AFTER the CURRENT JOB, ... UNLESS they are CAUGHT, in which case you
can skip the queue and get your feedback IMMEDIATELY in the SAME JOB.

```
console.log ('begin')
new Promise ( F => { throw Error } )
console.log (' ... in progress ... ')
```
###### Aside, on BROWSER implementations:

Browsers tend to have two job queues, a HIGH-priority MICROTASK queue, and a
LOW-priority MACROTASK queue. MACROtasks are jobs from big-world events
including user interface (UI) events, such as button clicks and keypresses.
MICROtasks are jobs from small-world events including coded ontologies, such as
Promises. When a MACROtask completes, the browser will try to complete ALL
outstanding MICROtasks, before proceeding to any outstanding MACROtask.
