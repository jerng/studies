- window.document.cookie: key-value string
- window.caches : keyed Response objects
- window.localStorage : 5 MiB : keyed strings
- window.indexedDB : “unlimited” : keyed JavaScript objects
- window.navigator.storage : “unlimited” : keyed files & directories

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

-

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

-

window.indexedDB
    // IDBFactory object 

-

IDBFactory.open( nameString, versionInteger )
    // returns IDBOpenDBRequest object ; if open() is successful,
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

IDBDatabase.name
    // nameString 

IDBDatabase.version
    // versionInteger

IDBDatabase.objectStoreNames
    // DOMStringList object

IDBDatabase.createObjectStore()
IDBDatabase.transaction()
IDBDatabase.close()
IDBDatabase.deleteObjectStore()

```

