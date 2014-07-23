#include "wayland-msg-handling.h"

#include <stdio.h>

#define MAX_FDS 28 /* this constant is from Wayland library */

static void build_cmsg(int *fds, int n_fds, char *data, int *clen)
{
    struct cmsghdr *cmsg;
    size_t size;

    if (n_fds > MAX_FDS)
        n_fds = MAX_FDS;

    size = n_fds * 4;

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

    cmsg = CMSG_FIRSTHDR(msg);

    while (cmsg) {
        int n_fds_in_cmsg = 0;

        if (cmsg->cmsg_level != SOL_SOCKET || cmsg->cmsg_type != SCM_RIGHTS) {
            cmsg = CMSG_NXTHDR(msg, cmsg);
            continue;
        }

        size = cmsg->cmsg_len - CMSG_LEN(0);

        n_fds_in_cmsg = size / sizeof(int32_t);

        if (bufsize < size) {
            /* TODO: close the fds */
            return -1;
        }

        memcpy(fdp, CMSG_DATA(cmsg), size);
        fdp += n_fds_in_cmsg;
        n += n_fds_in_cmsg;
        bufsize -= size;

        cmsg = CMSG_NXTHDR(msg, cmsg);
    }

    return n;
}

int recvmsg_wayland(int fd, const char *buf, int bufsize, int *fds,
        int fdbufsize, int *n_fds)
{
    char cmsg_buf[CMSG_LEN(MAX_FDS*4)];
    struct iovec iov[1];
    struct msghdr msg;
    int len;

    iov[0].iov_base = (void *) buf;
    iov[0].iov_len = bufsize;

    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg_buf;
    msg.msg_controllen = sizeof(cmsg_buf);
    msg.msg_flags = 0;

    do {
        len = recvmsg(fd, &msg, MSG_CMSG_CLOEXEC);
    } while (len == -1 && errno == EINTR);

    if (len >= 0)
        *n_fds = decode_cmsg(fds, fdbufsize, &msg);
    else {
        /* printf("recvmsg error: %m!\n"); */
        *n_fds = 0;
    }

    return len;
}

int sendmsg_wayland(int fd, const char *buf, int bufsize, int *fds, int n_fds)
{
    char cmsg_buf[CMSG_LEN(MAX_FDS*4)];
    struct iovec iov[1];
    struct msghdr msg;
    int clen, len, i;

    iov[0].iov_base = (void *) buf;
    iov[0].iov_len = bufsize;

    build_cmsg(fds, n_fds, cmsg_buf, &clen);

    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg_buf;
    msg.msg_controllen = clen;
    msg.msg_flags = 0;

    do {
        len = sendmsg(fd, &msg, MSG_NOSIGNAL | MSG_DONTWAIT);
    } while (len == -1 && errno == EINTR);

    if (len == -1) {
        /* printf("sendmsg error: %m!\n"); */
        return -1;
    }

    /* close the fds now */

    for (i = 0; i < n_fds; i++) {
        close(fds[i]);
    }

    return len;
}