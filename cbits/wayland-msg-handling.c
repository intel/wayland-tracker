#include "wayland-msg-handling.h"

#include <stdio.h>

#define MAX_FDS_OUT 28
#define CLEN        (CMSG_LEN(MAX_FDS_OUT * sizeof(int32_t)))

static void build_cmsg(int *fds, int n_fds, char *data, int *clen)
{
    struct cmsghdr *cmsg;
    size_t size;

    size = n_fds * sizeof(int32_t);
    if (size > MAX_FDS_OUT * sizeof(int32_t))
        size = MAX_FDS_OUT * sizeof(int32_t);

    if (size > 0) {
        cmsg = (struct cmsghdr *) data;
        cmsg->cmsg_level = SOL_SOCKET;
        cmsg->cmsg_type = SCM_RIGHTS;
        cmsg->cmsg_len = CMSG_LEN(size);
        memcpy(CMSG_DATA(cmsg), fds, size);
        *clen = cmsg->cmsg_len;
    } else {
        *clen = 0;
    }
}

static int decode_cmsg(int *fds, int bufsize, struct msghdr *msg)
{
    struct cmsghdr *cmsg;
    size_t size;
    int n = 0;
    int *fdp = fds;

    for (cmsg = CMSG_FIRSTHDR(msg); cmsg != NULL;
         cmsg = CMSG_NXTHDR(msg, cmsg)) {

        int n_fds_in_cmsg = 0;

        printf("CMSG block\n");

        if (cmsg->cmsg_level != SOL_SOCKET ||
            cmsg->cmsg_type != SCM_RIGHTS)
            continue;

        size = cmsg->cmsg_len - CMSG_LEN(0);

        printf("size == %ld, remaining space == %d\n", size, bufsize);

        n_fds_in_cmsg = size / sizeof(int32_t);

        if (bufsize < size) {
            /* TODO: close the fds */
            return -1;
        }

        printf("n_fds in this msg == %d\n", n_fds_in_cmsg);

        memcpy(fdp, CMSG_DATA(cmsg), size);
        fdp += n_fds_in_cmsg;
        n += n_fds_in_cmsg;
        bufsize -= size;
    }

    return n;
}

int recvmsg_wayland(int fd, const char *buf, int bufsize, int *fds,
        int fdbufsize, int *n_fds)
{
    char cmsg[CLEN];
    struct iovec iov[1];
    struct msghdr msg;
    int len;

    iov[0].iov_base = (void *) buf;
    iov[0].iov_len = bufsize;

    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg;
    msg.msg_controllen = sizeof(cmsg);
    msg.msg_flags = 0;

    do {
        len = recvmsg(fd, &msg, MSG_CMSG_CLOEXEC);
    } while (len == -1 && errno == EINTR);

    if (len >= 0)
        *n_fds = decode_cmsg(fds, fdbufsize, &msg);
    else {
        printf("recvmsg error: %m!\n");
        *n_fds = 0;
    }

    printf("recvmsg len %d\n", len);
    return len;
}

int sendmsg_wayland(int fd, const char *buf, int bufsize, int *fds, int n_fds)
{
    char cmsg[CLEN];
    struct iovec iov[1];
    struct msghdr msg;
    int clen, len, i;

    iov[0].iov_base = (void *) buf;
    iov[0].iov_len = bufsize;

    build_cmsg(fds, n_fds, cmsg, &clen);

    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg;
    msg.msg_controllen = clen;
    msg.msg_flags = 0;

    do {
        len = sendmsg(fd, &msg, MSG_NOSIGNAL | MSG_DONTWAIT);
    } while (len == -1 && errno == EINTR);

    if (len == -1)
        return -1;

    /* close the fds now */

    for (i = 0; i < n_fds; i++) {
        close(fds[i]);
    }

    return len;
}