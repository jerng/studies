//  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises


/* What a promise actually is. When you create a new Promise, you’re really just
 * creating a plain old JavaScript object. This object can invoke two methods,
 * then, and catch. Here’s the key. When the status of the promise changes to
 * fulfilled, the function that was passed to .then will get invoked. When the
 * status of a promise changes to rejected, the function that was passed to
 * .catch will be invoked. What this means is that once you create a promise,
 * you’ll pass the function you want to run if the async request is successful
 * to .then. You’ll pass the function you want to run if the async request fails
 * to .catch.
 *
 * FROM :
 * https://medium.com/@paulrohan/converting-javascript-callbacks-to-promise-and-async-await-replacing-async-waterfall-method-with-3c8b7487e0b9
 */






// This executor never resolves, and never rejects
//      ... resultingly, limboPromise.[[PromiseStatus]] is always "pending", and
//      limboPromise.[[PromiseValue]] is undefined
limboExecutor = (resolver, rejector) => {}
limboPromise  = new Promise( limboExecutor )

// This executor resolves immediately, but trivially
//      ... resultingly, trivialPromise.[[PromiseStatus]] will be "resolved",
//      and trivialPromise.[[PromiseValue]] is undefined
trivialExecutor = (resolver, rejector) => { resolver() }
trivialPromise  = new Promise( trivialExecutor )

// This executor resolves immediately,  returning the value '123' 
//      ... resultingly, trivialPromise.[[PromiseStatus]] will be "resolved",
//      and trivialPromise.[[PromiseValue]] is 123
trivialExecutor = (resolver, rejector) => { resolver(123) }
trivialPromise  = new Promise( trivialExecutor )

// This executor resolves after 500ms, returning the value '123' 
//      ... resultingly, usefulPromise.[[PromiseStatus]] will be "pending" until
//      500ms, after which it is "resolved" and trivialPromise.[[PromiseValue]] is 123
usefulExecutor = (resolver, rejector) => { 

    t1 = Date.now()
    setTimeout( 
        () =>   {   resolver (123)
                    console.log(`resolver ends; ${Date.now() - t1} ms later`)
                }, 
                500 
    )
    console.log('executor ends') 
}
usefulPromise  = new Promise( usefulExecutor )

// This executor rejects after 500ms, returning the value '123' 
//      ... resultingly, usefulPromise.[[PromiseStatus]] will be "pending" until
//      500ms, after which it is "rejected" and trivialPromise.[[PromiseValue]] is 123
usefulExecutor = (resolver, rejector) => { 

    t1 = Date.now()
    setTimeout( 
        () =>   {   rejector(123)
                    console.log(`rejector ends; ${Date.now() - t1} ms later`)
                }, 
                500 
    )
    console.log('executor ends') 
}
usefulPromise  = new Promise( usefulExecutor )




/* Understanding (Promise.resolve(x)): 
 *
 *  (return "aaa") 
 *      is the same as (return Promise.resolve("aaa"))
 *
 *    and 
 *  (return Promise.resolve("aaa"))
 *      is the same as (return Promise.resolve(Promise.resolve("aaa")))
 *
 *  From:
 *  https://stackoverflow.com/questions/27715275/whats-the-difference-between-returning-value-or-promise-resolve-from-then#comment43847145_27716590
 *
 *  So the following are all identical for a promise or plain value X:
 *  From: https://stackoverflow.com/a/27717844
 */

Promise.resolve(x);
new Promise(function(resolve, reject){ resolve(x); });
Promise.resolve().then(function(){ return x; });
Promise.all([x]).then(function(arr){ return arr[0]; });




/* Understanding (Promise.reject(x)): 
 *
 *  inside a then handler function:
 *
 *  A) When x is a value (number, string, etc):
 *
 *      return x is equivalent to return Promise.resolve(x)
 *
 *      throw x is equivalent to return Promise.reject(x)
 *
 *  B) When x is a Promise that is already settled (not pending anymore):
 *
 *      return x is equivalent to return Promise.resolve(x), if the Promise was
 *      already resolved.
 *  
 *      return x is equivalent to return Promise.reject(x), if the Promise was already
 *      rejected.
 *
 *  C) When x is a Promise that is pending:
 *
 *      return x will return a pending Promise, and it will be evaluated on the
 *      subsequent then.
 *
 *  FROM: https://stackoverflow.com/a/47168159
 */





