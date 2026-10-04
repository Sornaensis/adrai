#include <errno.h>
#include <sys/file.h>
#include <unistd.h>

/* flock authority belongs to this open file description, not a PID/token. */
int adrai_git_lock_try(int descriptor) {
    return flock(descriptor, LOCK_EX | LOCK_NB) == 0 ? 0 : -errno;
}

/* A safe FFI call keeps the owner alive until all writes and fsync finish. */
int adrai_git_lock_write(int descriptor, const void *bytes, size_t count) {
    if (lseek(descriptor, 0, SEEK_SET) < 0 || ftruncate(descriptor, 0) < 0) return -errno;
    const char *cursor = bytes;
    while (count > 0) {
        ssize_t written = write(descriptor, cursor, count);
        if (written < 0) { if (errno == EINTR) continue; return -errno; }
        if (written == 0) return -EIO;
        cursor += written;
        count -= (size_t) written;
    }
    while (fsync(descriptor) < 0) { if (errno != EINTR) return -errno; }
    return 0;
}
