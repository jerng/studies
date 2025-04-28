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
