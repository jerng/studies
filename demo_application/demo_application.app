{ application, 
  demo_application,
  [ { mod,  
      { demo_application,
        []
      }
    },
    { applications, 
      [ kernel,
        stdlib
      ]
    }
  ]
}.
