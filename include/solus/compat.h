// Compatibility files
#ifndef COMPAT_H
#define COMPAT_H

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <arpa/inet.h>

#if defined(_WIN32) || defined(POSIX_COMPAT)
static inline uint64_t htonll(uint64_t x) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return ((uint64_t)htonl((uint32_t)(x & 0xFFFFFFFFULL)) << 32) |
           htonl((uint32_t)(x >> 32));
#else
    return x;
#endif
}

static inline uint64_t ntohll(uint64_t x) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return ((uint64_t)ntohl((uint32_t)(x & 0xFFFFFFFFULL)) << 32) |
           ntohl((uint32_t)(x >> 32));
#else
    return x;
#endif
}

/// I use this too much to not implement it for all platforms
static inline char *strdup(const char *s) {
    size_t len = strlen(s) + 1;
    char *out = malloc(len);
    if (!out) return NULL;
    memcpy(out, s, len);
    return out;
}
#endif

#endif // COMPAT_H
