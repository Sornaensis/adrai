#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

/* A fresh open description preserves the pinned directory, without sharing
 * the caller's enumeration offset. All layout/errno details use host headers. */
void *adrai_watch_directory_open(int parent, int *error) {
    int fd = openat(parent, ".", O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) { *error = errno; return NULL; }
    DIR *stream = fdopendir(fd);
    if (!stream) { *error = errno; close(fd); }
    return stream;
}

int adrai_watch_directory_next(void *stream, void **name) {
    errno = 0;
    struct dirent *entry = readdir((DIR *)stream);
    if (!entry) return errno ? -errno : 0;
    *name = entry->d_name;
    return 1;
}

int adrai_watch_directory_close(void *stream) {
    return closedir((DIR *)stream) == 0 ? 0 : -errno;
}
