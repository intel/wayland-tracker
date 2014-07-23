#include <sys/types.h>
#include <sys/socket.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int recvmsg_wayland(int fd, const char *buf, int bufsize, int *fds,
        int fdbufsize, int *n_fds);

int sendmsg_wayland(int fd, const char *buf, int bufsize, int *fds, int n_fds);
