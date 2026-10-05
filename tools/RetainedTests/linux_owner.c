#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <sched.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/socket.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

/* The native creating thread stays alive. Neither a .NET thread nor a PID
 * search is ownership authority. PID1 never execs the payload. */
enum { READY = 1, EXECUTED, ROOT_EXIT, LAUNCH_ERROR };
struct event { int kind; int value; };
static volatile sig_atomic_t stopping;
static int wake_writer = -1;
static void stop_signal(int ignored) {
    int error = errno;
    (void)ignored; stopping = 1;
    if (wake_writer >= 0) { char byte = 'S'; (void)write(wake_writer, &byte, 1); }
    errno = error;
}
static int pidfd_open_self(void) { return (int)syscall(SYS_pidfd_open, getpid(), 0); }
static int kill_namespace(int fd) {
    if (syscall(SYS_pidfd_send_signal, fd, SIGKILL, NULL, 0) == 0) return 0;
    /* ESRCH still needs the exact pidfd and waitpid/reaping observation. */
    if (errno == ESRCH) return 0;
    printf("FAULT %d\n", errno); fflush(stdout);
    return -1;
}
static int send_event(int fd, int kind, int value) {
    struct event e = { kind, value };
    ssize_t n;
    do { n = send(fd, &e, sizeof(e), MSG_NOSIGNAL); } while (n < 0 && errno == EINTR);
    return n == (ssize_t)sizeof(e) ? 0 : -1;
}
static int write_map(const char *path, const char *text) {
    int fd = open(path, O_WRONLY | O_CLOEXEC);
    if (fd < 0) return -1;
    size_t length = strlen(text), at = 0;
    while (at < length) {
        ssize_t n = write(fd, text + at, length - at);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) { int error = n < 0 ? errno : EIO; close(fd); errno = error; return -1; }
        at += (size_t)n;
    }
    return close(fd);
}
static int map_identity(uid_t uid, gid_t gid) {
    char text[80];
    if (write_map("/proc/self/setgroups", "deny\n") < 0) return -1;
    snprintf(text, sizeof(text), "0 %lu 1\n", (unsigned long)uid);
    if (write_map("/proc/self/uid_map", text) < 0) return -1;
    snprintf(text, sizeof(text), "0 %lu 1\n", (unsigned long)gid);
    return write_map("/proc/self/gid_map", text);
}
static int exited_code(int status) {
    return WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
}
static void namespace_init(int channel, int parent_fd, int out, int err, char **argv) {
    /* Credentials must be final before arming PDEATHSIG. A pidfd opened by
     * the native parent before fork closes the already-dead-before-arm race. */
    close(STDIN_FILENO); /* Only the controller holds the death-channel reader. */
    /* PID1 must not retain the controller's status/diagnostic pipe writers.
     * Reserve standard descriptors so the exec-error pipe cannot alias them. */
    int quiet = open("/dev/null", O_RDWR | O_CLOEXEC);
    if (quiet < 0 || dup2(quiet, STDOUT_FILENO) < 0 || dup2(quiet, STDERR_FILENO) < 0 ||
        fcntl(STDIN_FILENO, F_SETFD, 0) < 0) goto failed;
    char activate;
    ssize_t activation;
    do { activation = recv(channel, &activate, 1, 0); } while (activation < 0 && errno == EINTR);
    if (activation != 1 || activate != 'A') _exit(125);
    if (setresgid(0, 0, 0) < 0 || setresuid(0, 0, 0) < 0 ||
        prctl(PR_SET_PDEATHSIG, SIGKILL) < 0) goto failed;
    struct pollfd owner = { parent_fd, POLLIN, 0 };
    if (poll(&owner, 1, 0) < 0 || owner.revents != 0) _exit(125);
    if (send_event(channel, READY, 0) < 0) _exit(125);
    char go;
    ssize_t n;
    do { n = recv(channel, &go, 1, 0); } while (n < 0 && errno == EINTR);
    if (n != 1 || go != 'G') _exit(125);
    int errors[2];
    if (pipe2(errors, O_CLOEXEC) < 0) goto failed;
    pid_t payload = fork();
    if (payload < 0) { int error = errno; close(errors[0]); close(errors[1]); errno = error; goto failed; }
    if (payload == 0) {
        close(errors[0]);
        close(channel);
        close(parent_fd);
        int null_input = open("/dev/null", O_RDONLY | O_CLOEXEC);
        if (null_input < 0 || dup2(null_input, STDIN_FILENO) < 0 ||
            dup2(out, STDOUT_FILENO) < 0 || dup2(err, STDERR_FILENO) < 0 ||
            fcntl(STDIN_FILENO, F_SETFD, 0) < 0) {
            int error = errno; (void)write(errors[1], &error, sizeof(error)); _exit(127);
        }
        if (null_input > 2) close(null_input);
        if (out > 2) close(out);
        if (err > 2) close(err);
        execv(argv[0], argv);
        int error = errno;
        (void)write(errors[1], &error, sizeof(error));
        _exit(127);
    }
    /* No PID1 copy may keep a payload output or exec-ack pipe open. */
    close(out); close(err); close(errors[1]);
    int launch_error = 0;
    do { n = read(errors[0], &launch_error, sizeof(launch_error)); } while (n < 0 && errno == EINTR);
    close(errors[0]);
    if (n < 0 || (n != 0 && n != (ssize_t)sizeof(launch_error))) {
        (void)send_event(channel, LAUNCH_ERROR, n < 0 ? errno : EIO);
    } else if (n != 0) {
        (void)send_event(channel, LAUNCH_ERROR, launch_error);
    } else if (send_event(channel, EXECUTED, 0) < 0) _exit(125);
    for (;;) {
        int status;
        pid_t child = waitpid(-1, &status, 0);
        if (child < 0 && errno == EINTR) continue;
        if (child < 0 && errno == ECHILD) break;
        if (child < 0) goto failed;
        if (child == payload && send_event(channel, ROOT_EXIT, exited_code(status)) < 0) _exit(125);
    }
    close(channel); close(parent_fd);
    _exit(0);
failed:
    (void)send_event(channel, LAUNCH_ERROR, errno);
    _exit(125);
}
int main(int argc, char **argv) {
    int child_created = 0;
    if (argc < 5) { printf("ERROR %d\nTREE\n", EINVAL); return 2; }
    uid_t uid = getuid(); gid_t gid = getgid();
    int parent_fd = pidfd_open_self();
    if (parent_fd < 0) goto initial_failed;
    int out = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC, 0600);
    int err = open(argv[3], O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC, 0600);
    if (out < 0 || err < 0 || chdir(argv[1]) < 0) goto initial_failed;
    int channel[2];
    if (socketpair(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0, channel) < 0) goto initial_failed;
    /* Exact previously admitted flags; no mount/network/cgroup namespace. */
    if (unshare(CLONE_NEWUSER | CLONE_NEWPID) < 0 || map_identity(uid, gid) < 0) goto initial_failed;
    struct sigaction action;
    int wake[2];
    if (pipe2(wake, O_CLOEXEC | O_NONBLOCK) < 0) goto initial_failed;
    wake_writer = wake[1];
    memset(&action, 0, sizeof(action)); action.sa_handler = stop_signal;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGTERM, &action, NULL) < 0 || sigaction(SIGINT, &action, NULL) < 0) goto initial_failed;
    pid_t init = fork();
    if (init < 0) goto initial_failed;
    if (init == 0) {
        close(wake[0]); close(wake[1]); wake_writer = -1;
        close(channel[0]); namespace_init(channel[1], parent_fd, out, err, argv + 4);
    }
    child_created = 1;
    close(channel[1]); close(parent_fd); close(out); close(err);
    int init_fd = (int)syscall(SYS_pidfd_open, init, 0);
    /* An unreaped child cannot be recycled, so this initial identity is stable. */
    if (init_fd < 0) {
        int error = errno; kill(init, SIGKILL);
        pid_t joined; do { joined = waitpid(init, NULL, 0); } while (joined < 0 && errno == EINTR);
        if (joined == init) child_created = 0;
        errno = error; goto initial_failed;
    }
    int root_seen = 0, ready_seen = 0, exec_seen = 0, error_seen = 0, cancelled = 0;
    int activated = 0;
    int channel_open = 1;
    int control_open = 1, stop_attempted = 0;
    printf("CREATED %ld\n", (long)init); fflush(stdout);
    for (;;) {
        struct pollfd fds[4] = { { control_open ? STDIN_FILENO : -1, POLLIN, 0 }, { channel_open ? channel[0] : -1, POLLIN, 0 }, { init_fd, POLLIN, 0 }, { wake[0], POLLIN, 0 } };
        if (stopping && !stop_attempted) { cancelled = kill_namespace(init_fd) == 0; stop_attempted = 1; }
        int result = poll(fds, 4, -1);
        if (result < 0 && errno == EINTR) continue;
        if (result < 0 && !stop_attempted) { cancelled = kill_namespace(init_fd) == 0; stop_attempted = 1; }
        if (result >= 0 && fds[3].revents != 0) { char bytes[32]; while (read(wake[0], bytes, sizeof(bytes)) > 0) {} continue; }
        if (result >= 0 && fds[0].revents != 0) {
            char command;
            ssize_t n = read(STDIN_FILENO, &command, 1);
            if (n == 1 && command == 'A' && !activated && !stop_attempted) {
                if (send(channel[0], &command, 1, MSG_NOSIGNAL) == 1) activated = 1;
                else { cancelled = kill_namespace(init_fd) == 0; stop_attempted = 1; }
            } else if (n == 1 && command == 'G' && activated && ready_seen && !stop_attempted) {
                if (send(channel[0], &command, 1, MSG_NOSIGNAL) != 1 && !stop_attempted) { cancelled = kill_namespace(init_fd) == 0; stop_attempted = 1; }
            } else if (!stop_attempted) { cancelled = kill_namespace(init_fd) == 0; stop_attempted = 1; }
            if (n <= 0 || (command != 'G' && command != 'A')) { close(STDIN_FILENO); control_open = 0; }
        }
        if (result >= 0 && (fds[1].revents != 0 || fds[2].revents != 0) && channel_open) {
          for (;;) {
            struct event e;
            ssize_t n = recv(channel[0], &e, sizeof(e), MSG_DONTWAIT);
            if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) break;
            if (n < 0 && errno == EINTR) continue;
            if (n == 0) { close(channel[0]); channel_open = 0; }
            else if (n != (ssize_t)sizeof(e)) {
                if (!stop_attempted) { cancelled = kill_namespace(init_fd) == 0; stop_attempted = 1; }
            }
            else {
                if (e.kind == READY && !ready_seen) { ready_seen = 1; puts("READY"); }
                else if (e.kind == EXECUTED && ready_seen && !exec_seen) { exec_seen = 1; puts("EXEC"); }
                else if (e.kind == ROOT_EXIT && !root_seen) { root_seen = 1; printf("ROOT %d\n", e.value); }
                else if (e.kind == LAUNCH_ERROR && !error_seen) { error_seen = 1; printf("ERROR %d\n", e.value); }
                else if (!stop_attempted) { cancelled = kill_namespace(init_fd) == 0; stop_attempted = 1; }
                fflush(stdout);
            }
            if (!channel_open || n != (ssize_t)sizeof(e)) break;
          }
        }
        if (result >= 0 && fds[2].revents != 0) {
            int status;
            while (waitpid(init, &status, 0) < 0) { if (errno != EINTR) goto initial_failed; }
            /* Reaping namespace PID1 is kernel-backed tree absence. */
            if (!root_seen && !cancelled && !error_seen) printf("ERROR %d\n", ECHILD);
            puts("TREE"); fflush(stdout);
            if (channel_open) close(channel[0]);
            close(init_fd);
            return 0;
        }
    }
initial_failed:
    printf("ERROR %d\n", errno);
    if (!child_created) puts("TREE");
    fflush(stdout);
    return 2;
}
