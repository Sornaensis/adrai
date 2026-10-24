#ifdef _WIN32
#include <winsock2.h>
#include <stdint.h>

/* No concurrent close/cancel: the caller owns this nonblocking operation until
 * select completes, then restores blocking mode before the socket is reused. */
int adrai_connect_bounded(int fd, const void *address, int length, int timeout_ms) {
  SOCKET socket = (SOCKET)(uintptr_t)(unsigned int)fd;
  u_long nonblocking = 1;
  int result, error = 0, error_length = sizeof(error);
  fd_set writable, failed;
  struct timeval deadline;
  if (ioctlsocket(socket, FIONBIO, &nonblocking) != 0) return WSAGetLastError();
  result = connect(socket, (const struct sockaddr *)address, length);
  if (result == SOCKET_ERROR) {
    error = WSAGetLastError();
    if (error == WSAEWOULDBLOCK || error == WSAEINPROGRESS) {
      FD_ZERO(&writable);
      FD_ZERO(&failed);
      FD_SET(socket, &writable);
      FD_SET(socket, &failed);
      deadline.tv_sec = timeout_ms / 1000;
      deadline.tv_usec = (timeout_ms % 1000) * 1000;
      result = select(0, NULL, &writable, &failed, &deadline);
      if (result == 0) error = WSAETIMEDOUT;
      else if (result == SOCKET_ERROR) error = WSAGetLastError();
      else if (getsockopt(socket, SOL_SOCKET, SO_ERROR, (char *)&error, &error_length) != 0)
        error = WSAGetLastError();
    }
  }
  nonblocking = 0;
  if (ioctlsocket(socket, FIONBIO, &nonblocking) != 0 && error == 0)
    error = WSAGetLastError();
  return error;
}
#endif
