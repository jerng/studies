- window.document.cookie: key-value string
- window.caches : keyed Response objects
- window.localStorage : 5 MiB : keyed strings
- window.indexedDB : “unlimited” : keyed JavaScript objects
- window.navigator.storage : “unlimited” : keyed files & directories

### window.caches
```
window.caches
    // Promise ( CacheStorage object )
- 

CacheStorage.open   ( stringCacheName )
    // returns Promise ( Cache object ) ... creates new, if none found

CacheStorage.keys   ( )  
    // returns Promise ( [ stringCacheNames ] )

CacheStorage.has    ( stringCacheName )     
    // returns Promise ( Boolean )

CacheStorage.match  ( stringCacheName )   
    // returns Promise ( Cache object )

CacheStorage.delete ( stringCacheName )  
    // returns Promise ( Boolean )
-

Cache.add( RequestObject | urlString )
    // returns Promise ( undefined )

Cache.addAll( [ urlStrings ] )
    // returns Promise ( undefined )

Cache.put( RequestObject | urlString, ResponseObject )
    // returns Promise ( undefined )
    //
    //  ResponseObject can be synthesised via 
    //      new Response ( bodyString )

Cache.keys( RequestObject | urlString, optionsObject )
    // returns Promise ( [ RequestObjects ] )

Cache.match( RequestObject | urlString, optionsObject )
    // returns Promise ( first matched ResponseObject | undefined )

Cache.matchAll( RequestObject | urlString, optionsObject  )
    // returns Promise ( [ ResponseObject matches ] )

Cache.delete( RequestObject | urlString )
    // returns Promise ( Boolean )
```

### window.localStorage
```
window.localStorage
    // Storage object
-

Storage.length
    // integer count of key-value pairs stored

Storage.setItem( keyString, valueString )
    // returns undefined

Storage.key( storedItemIndexInteger )
    // returns keyString

Storage.getItem( keyString)
    // returns valueString

Storage.removeItem( keyString )
    // returns undefined

Storage.clear()
    // returns undefined ; removes all key-value pairs
```

### window.indexedDB
```
window.indexedDB
    // IDBFactory object ;
```

the critical path is : ( pseudocode )
```
db = window.indexedDB.open()
t = db.transaction()
os = t.objectStore()
os.doAllTheThings()
t.commit()
db.close()
```
```javascript
/*  1. This requests an IndexedDB connection, allowing reuse */
const dbRequestPromise =(_=>{
  const request = indexedDB.open('exampleDatabase');
  request.onupgradeneeded =_=>{
      /* db.createObjectStore() may be used only in this callback */ 
  }
  return request 
})();

/*  
 *  2. This is a utility function, 
 *      for getting IDBResults from IDBRequests.
 *
 *  Add `reject` for a less minimal example.
 *
 *  Why call it `fmap`? See :
 *  https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html  
 *
 */
const fmapGetResult = resolveIDBRequestResultOnSuccess = 
      idbr=>new Promise( resolve=>idbr.onsuccess=_=>resolve(idbr.result) );

/* 
 *  3. Here's how you use the DB connection.
 *  You can have any number of such IIFEs, 
 *      but they must be `async` because they must `await`.
 */
(async _=>{ 
    const db = await fmapGetResult( dbRequestPromise ); 
    /* use db here */ 
})();

```
```
IDBFactory.open( nameString, versionInteger )
    // returns Promise ( IDBOpenDBRequest object ) ; if open() is successful,
    // IDBOpenDBRequest.result == IDBDatabase object

IDBFactory.databases()
    // returns Promise ( [ { name: nameString, version: versionInteger ] ) 

IDBFactory.deleteDatabase( nameString )
    // returns IDBOpenDBRequest object ; if deleteDatabase() is
    // successful, IDBOpenDBRequest.result == null

IDBFactory.cmp( keyOne, keyTwo )
    // valid key types are : String, Date, Float, Blob, Array
    // returns  -1 : keyOne <  keyTwo
    //           0 : keyOne == keyTwo
    //           1 : keyOne >  keyTwo
-

IDBDatabase
    // represents a CONNECTION to a database

IDBDatabase.name
    // nameString 

IDBDatabase.version
    // versionInteger

IDBDatabase.objectStoreNames
    // DOMStringList object

IDBDatabase.createObjectStore( objectStoreNameString, optionsObject )
    // nameStrings = "" is allowed ( but not advised )
    // optionsObject.autoIncrement defaults to false. 
    // optionsObject.keyPath 
    //      : if not specified, the objectStore will use out-of-line keys 
    //      : inline-keys may be specified as, 
    //          an empty string, 
    //          a JavaScript identifier, or 
    //          multiple JavaScript identifiers separated by periods, or
    //          an array containing any of those. 
    //          It cannot include spaces.
    // returns IDBObjectStore object

IDBDatabase.transaction( [ objectStoreNameStrings ] , "readonly" ( default ) | "readwrite", optionsObject )
    //  optionsObject.durability = 
    //      "strict" ( recommended ) | "relaxed" ( faster, less energy consumed )
    //      | "default" ( user agent decides )
    //  the order of calls to transaction() determine the ORDER OF
    //      transaction EXECUTIONS ;
    // returns IDBTransaction object ; this is the ONLY WAY 

IDBDatabase.close()
    // returns undefined ; but executes the close in a separate thread

IDBDatabase.deleteObjectStore( objectStoreStringName )
    // returns undefined
-

IDBTransaction.db
    // returns the IDBDatabase object

IDBTransaction.durability
    // returns the optionsDurabilityString

IDBTransaction.error
    // returns a DOMException object ( failure ) or null ( success )

IDBTransaction.mode
    // returns the optionsModeString

IDBTransaction.objectStoreNames
    // returns the DOMStringList

IDBTransaction.abort()
    // returns undefined

IDBTransaction.objectStore( objectStoreNameString )
    // returns the IDBObjectStore

IDBTransaction.commit()
    //  the order of calls to IDBDatabase.transaction() determines the
    //      ORDER OF transaction EXECUTIONS ;
    // returns undefined
-

IDBObjectStore.indexNames
    // [ indexNameStrings ]

IDBObjectStore.keyPath
    // see IDBDatabase.createObjectStore()

IDBObjectStore.name
    // objectStoreNameString

IDBObjectStore.transaction
    // the parent IDBTransaction 

IDBObjectStore.autoIncrement
    // Boolean, optionsObject.autoIncrement

IDBObjectStore.add( value, key=null )
    // returns IDBRequest object, 
    // where IDBRequest.result is the new record key

IDBObjectStore.clear()
    // returns IDBRequest object,
    // where IDBRequest.result is undefined, upon success ;
    // but executes deletion of every object in the store in a separate thread

IDBObjectStore.count( key | IDBKeyRange object | null )
    // returns IDBRequest object,
    // where IDBRequest.result is an Integer, upon success

IDBObjectStore.createIndex( indexNameString, keyPath, optionsObject=null )
    // optionsObject.unique accepts a Boolean, defaults to false 
    // optionsObject.multiEntry accepts a Boolean, defaults to false 
    // returns a IDBIndex object

IDBObjectStore.delete( key | IDBKeyRange object )
    // less efficient than IDBCursor.delete()
    // returns IDBRequest object,
    // where IDBRequest.result is undefined, upon success ;

IDBObjectStore.deleteIndex( indexStringName )
    // returns undefined

IDBObjectStore.get( key | IDBKeyRange object )
    // returns IDBRequest object, where IDBRequest.result is a
    // STRUCTURED CLONE of the first VALUE that matches, upon success ;
    // WARNING : will return undefined for BOTH unmatched keys, and keys
    // with a the value undefined ; to differentiate, use openCursor()

IDBObjectStore.getKey( key | IDBKeyRange object )
    // returns IDBRequest object, where IDBRequest.result is a
    // STRUCTURED CLONE of the first KEY that matches, upon success ;
    // ( CHECK: inconsistent documentation with getAllKeys() )

IDBObjectStore.getAll( key | IDBKeyRange | null, count=null)
    // will select all records, if key is not provided;
    // returns IDBRequest object, where IDBRequest.result is an
    // Array containing STRUCTURED CLONES of the VALUES that match, upon success ;
    // WARNING : will return undefined for BOTH unmatched keys, and keys
    // with a the value undefined ; to differentiate, use openCursor(),
    // or count()

IDBObjectStore.getAllKeys( key | IDBKeyRange | null, count=null)
    // will select all records, if key is not provided;
    // returns IDBRequest object, where IDBRequest.result is an
    // Array containing STRUCTURED CLONES of the KEYS that match, upon success ;
    // WARNING : will return undefined for BOTH unmatched keys, and keys
    // with a the value undefined ; to differentiate, use openCursor(),
    // or count()
    // ( CHECK: inconsistent documentation with getKeys() )

IDBObjectStore.index( indexNameString )
    // returns an IDBIndex object

IDBObjectStore.openCursor( key | IDBKeyRange | null, 
    "next" | "nextunique" | "prev" | "prevunique" )
    // "next" : cursor opens at the START of store, returns all VALUES,
    //      including duplicates, in INCREASING order of KEYS
    // "nextunique" : cursor opens at the START of store, returns all
    //      unique VALUES, in INCREASING order of KEYS
    // "prev" : cursor opens at the END of store, returns all VALUES,
    //      including duplicates, in DECREASING order of KEYS
    // "prevunique" : cursor opens at the END of store, returns all
    //      UNIQUE VALUES, in DECREASING order of KEYS
    // returns IDBRequest object, where IDBRequest.result is an
    //      IDBCursorWithValue object, pointing at the first record
    //      matched, upon success ; or null, if there were no matches

IDBObjectStore.openKeyCursor( key | IDBKeyRange | null, 
    "next" | "nextunique" | "prev" | "prevunique" )
    // for directional string arguments, see openCursor(), except that
    //      KEYS are returned instead of VALUES ;
    // returns IDBRequest object, where IDBRequest.result is an
    //      IDBCursor object, pointing at the first record
    //      matched, upon success ; or null, if there were no matches

IDBObjectStore.put( item, key=null )
    // returns IDBRequest object, where IDBRequest.result is the KEY for
    // the NEW or UPDATED record
-

```

