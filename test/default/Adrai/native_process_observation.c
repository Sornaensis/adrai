#include <stdint.h>
#if defined(_WIN32)
#include <windows.h>
int64_t adrai_test_process_pin(int pid) {
    HANDLE handle = OpenProcess(SYNCHRONIZE, FALSE, (DWORD)pid);
    return handle ? (int64_t)(intptr_t)handle : -1;
}
int adrai_test_process_exited(int64_t handle) {
    DWORD result = WaitForSingleObject((HANDLE)(intptr_t)handle, 0);
    return result == WAIT_OBJECT_0 ? 1 : result == WAIT_TIMEOUT ? 0 : -1;
}
int adrai_test_process_close(int64_t handle) {
    return CloseHandle((HANDLE)(intptr_t)handle) ? 0 : -1;
}
#elif defined(__linux__)
#include <unistd.h>
#include <sys/syscall.h>
#include <poll.h>
int64_t adrai_test_process_pin(int pid) {
#ifdef SYS_pidfd_open
    return syscall(SYS_pidfd_open, pid, 0);
#else
    return -1;
#endif
}
int adrai_test_process_exited(int64_t handle) {
    struct pollfd descriptor = { .fd = (int)handle, .events = POLLIN };
    int result = poll(&descriptor, 1, 0);
    if (result < 0 || (descriptor.revents & POLLNVAL)) return -1;
    return (descriptor.revents & POLLIN) ? 1 : 0;
}
int adrai_test_process_close(int64_t handle) { return close((int)handle); }
#else
#error Native fixture observation supports Windows and Linux only
#endif
