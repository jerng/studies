#### Install : GNU Ada : GNAT on Ubuntu
```
sudo apt install gnat
```

#### Code Sample 

`hello.adb` [source](https://en.wikibooks.org/wiki/Ada_Programming/Basic) :
```ada 
with Ada.Text_IO;

procedure Hello is
begin
   Ada.Text_IO.Put_Line("Hello, world!");
      end Hello;

```

#### Compile and Execute
```
gnatmake hello.adb
./hello
```

# Next : Sockets 

See `GNAT.Sockets` [example for HTTP
GET](http://gist.github.com/SevanBadal/64736b0f218af7fecd5e7b9315bde793)
