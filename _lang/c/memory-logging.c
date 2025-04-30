#include <stdio.h>
#include <sys/sysinfo.h>

int main() {
    struct sysinfo info;

    if (sysinfo(&info) != 0) {
        perror("sysinfo");
        return 1;
    }

    printf("Total RAM: %lu MB\n", info.totalram/1048576);
    printf("Free RAM: %lu MB\n", info.freeram/1048576);
    printf("Used RAM: %lu MB\n", (info.totalram - info.freeram)/1048576);
    printf("Total swap space size: %lu MB\n", info.totalswap/1048576);
    printf("Free swap space size: %lu MB\n", info.freeswap/1048576);

    return 0;
}
