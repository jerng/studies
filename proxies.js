So... sneaky feature warning... if you proxy an object, beware that the runtime
can make calls to (Proxy (target)[Symbol.toPrimitive] if at any time the (Proxy
(target)) is coerced... and YOUR custom getHandler will receive that call.

https://javascript.info/object-toprimitive
