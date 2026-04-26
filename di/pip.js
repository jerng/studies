/*  A POLICY INFORMATION POINT definition :
 * */
class PIP () {

    fnAllowances = {
        fnId0 : {

            access : Function,

            prerun : {

                callers : {
                    key0 : {
                        id : String
                        /* etc. */
                    } 
                },

                callees : {
                    key0 : {
                        id : String,
                        signature : {
                            in : { 
                                key0 : { 
                                    schema : Object 
                                } 
                                /* etc. */ 
                            }
                        }
                        /* etc. */
                    } 
                }
            },

            runtime : {

                callees : {
                    key0 : {
                        id : String,
                        signature : {
                            in : { 
                                key0 : { 
                                    schema : Object 
                                } 
                                /* etc. */ 
                            }
                        }
                        /* etc. */
                    } 
                }
            }
        }
    }

}
