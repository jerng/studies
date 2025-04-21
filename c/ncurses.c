#include <ncurses.h>
#include <time.h>

int main()
{ 
    initscr();      /* Start curses mode      */

    for(int k=0;k<1000000;k++)
    {
      for(int i=0;i<10;i++)
      {
        for(int j=0;j<10;j++)
        {
          printw("%5d",k); 
        }
        printw("\n"); 
      }
      refresh();
      move(0,0);
      nanosleep((struct timespec[]){{0, 100000000}}, NULL);
    }
    
    getch();      /* Wait for user input */
    endwin();     /* End curses mode      */

    return 0;
}
