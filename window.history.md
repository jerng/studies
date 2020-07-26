History API

    window.history
    -   this object is the access point for the API
    -   this object is read-only
    
    window.history.state
    -   this is the CURRENT STATE in the history stack (default: null)
    
    window.history.length
    -   this is the number of STATEs in the history stack (a new tab has 
        window.history.length=1)
    -   for the purpose of this document, we can refer to the integer range 
        [0, window.history.length] as the set of STATE_INDICES, where the 
        CURRENT_INDEX may be any one of these STATE_INDICES

    window.history.pushState ({someKey:'someValue'},null,'somePath?someQuery')
    -   sets LOCATIONBAR to : ORIGIN/somePath?someQuery
    -   pushes {someKey:'someValue'} upon the history stack
        -   consequently, window.history.length increases by 1

    window.history.replaceState ({someKey:'someValue'},null,'somePath?someQuery')
    -   sets LOCATIONBAR to : ORIGIN/somePath?someQuery
    -   pops the top state, off the history stack
    -   pushes {someKey:'someValue'}, upon the history stack
        -   consequently, window.history.length does not change
        
    WARNING : it is NOT POSSIBLE to discover the CURRENT_INDEX of the current state
        
    window.history.forward()
    -   same as the forward BUTTON
    -   replaces the CURRENT STATE with the STATE at (CURRENT_INDEX + 1), if
        that index exists; if it does not exist, nothing happens
        
    window.history.back()
    -   same as the back BUTTON
    -   replaces the CURRENT STATE with the STATE at (CURRENT_INDEX - 1), if
        that index exists; if it does not exist, nothing happens
        
    window.history.go(N)
    -   replaces the CURRENT STATE with the STATE at (CURRENT_INDEX + N), if
        that index exists; if it does not exist, nothing happens
    -   window.history.go(1) is the same as window.history.forward()
    -   window.history.go(-1) is the same as window.history.back()
    
    RELEVANT EVENTS
    -   popstate
    -   pageshow
    -   pagehide
    -   hashchange
