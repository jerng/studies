%%%-----------------------------------------------------------------------------
-module(o).
-export([test/0,new/0,test0/1]).

test0(Iterations)->
  lists:seq(0,Iterations).

test()->
  Object = o:new(),

  Object0 = Object({set,{property1,a}}),

  [ { parent, 
      { { self,     Object({self})     },
        { rubbish,  Object({rubbish})    },
        { set,      Object({set,{property1,a}})}
      }
    },
    { child, 
      { { self,     Object0({self})     },
        { rubbish,  Object0({rubbish})    },
        { property1,Object0({property1}) }
      }
    }
  ].

new()->
  fun
    (Input1)->
      ParentY = 
      fun 
        (ParentSelf,{ParentMethod}) -> 
          case ParentMethod of 
            self -> ParentSelf; 
            _ -> undefined 
          end;
        (ParentSelf,{set,{Property,Value}}) ->
          Child = 
          % this could probably be spun off
          fun 
            (Input2) -> 
              ChildY = 
              fun 
                (ChildSelf,ChildInput)->
                  case ChildInput of 
                    {Property} -> Value; 
                    _ ->  ParentSelf(ChildSelf,ChildInput) 
                  end 
              end,
              ChildY(ChildY,Input2) 
          end,
          Child 
      end,
      case Input1 of 
        {Method1} -> ParentY(ParentY,{Method1});
        {Method1,{Arg1,Arg2}} -> ParentY(ParentY,{Method1,{Arg1,Arg2}}) 
      end
  end.
