/*
 * Ignis runtime — filesystem helpers  (std/runtime/internal/rt_fs_win.c)
 *
 * Windows implementation — compiled only on _WIN32 platforms.
 *
 * Provides the same entry points as rt_fs.c (POSIX) but uses UCRT / Win32:
 *
 *   ignis_stat_win  — uses _stat64 to extract file metadata
 *   ignis_open3     — uses _open with mode (UCRT)
 *
 * Called from: std/fs/sys/windows.ign (Fs::Sys namespace).
 * Declared in: std/runtime/ignis_rt.h (bottom of file).
 */

#ifdef _WIN32

#include "../ignis_rt.h"
#include "rt_internal.h"

#include <sys/stat.h>
#include <sys/types.h>
#include <io.h>
#include <fcntl.h>
#include <errno.h>

/* S_IFMT / S_IFREG / S_IFDIR may be defined in sys/stat.h on UCRT */

// =============================================================================
// stat helper (Windows _stat64)
// =============================================================================

/*
 * Call _stat64 on `path` and extract fields into output pointers.
 * Returns 0 on success, -1 on failure (errno is set by UCRT).
 *
 * Windows _stat64 provides: st_mode, st_size, st_atime, st_mtime, st_ctime.
 * Fields without a Windows equivalent (dev, ino, nlink, uid, gid, blksize,
 * blocks) are written as 0.
 */
int ignis_stat_win(
    const char *path,
    u32 *out_mode,
    i64 *out_size,
    i64 *out_atime,
    i64 *out_mtime,
    i64 *out_ctime
) {
    struct __stat64 st;
    int rc = _stat64(path, &st);
    if (rc != 0) return -1;

    *out_mode  = (u32)st.st_mode;
    *out_size  = (i64)st.st_size;
    *out_atime = (i64)st.st_atime;
    *out_mtime = (i64)st.st_mtime;
    *out_ctime = (i64)st.st_ctime;
    return 0;
}

// =============================================================================
// open with mode (3-arg _open)
// =============================================================================

/*
 * Three-argument _open wrapper for file creation with mode.
 * On Windows, mode is typically _S_IREAD | _S_IWRITE.
 */
int ignis_open3(const char *pathname, int flags, u32 mode) {
    return _open(pathname, flags, (int)mode);
}

#endif /* _WIN32 */
