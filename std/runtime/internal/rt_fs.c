/*
 * Ignis runtime — filesystem helpers  (std/runtime/internal/rt_fs.c)
 *
 * POSIX implementation — compiled only on non-Windows platforms.
 *
 * Six thin C functions that cannot be expressed in pure Ignis today:
 *
 *   ignis_stat_call   — calls stat(2), extracts struct stat fields
 *   ignis_fstat_call  — calls fstat(2), extracts struct stat fields
 *   ignis_open3       — 3-arg open(2) (Ignis has no variadic calls)
 *   ignis_dirent_name — extracts d_name from struct dirent
 *   ignis_dirent_ino  — extracts d_ino  from struct dirent
 *   ignis_dirent_type — extracts d_type from struct dirent
 *
 * The stat/dirent extractors exist because struct stat and struct dirent
 * layouts are platform-dependent — Ignis records compile to sequential
 * C struct fields (field_0, field_1, ...) and cannot safely mirror the
 * real layout.  Each extractor writes individual scalars through output
 * pointers so the Ignis side only sees primitive types.
 *
 * Called from: std/fs/sys/unix.ign (Fs::Sys::Unix namespace).
 * Declared in: std/runtime/ignis_rt.h (bottom of file).
 */

#ifndef _WIN32

#include "../ignis_rt.h"
#include "rt_internal.h"

#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

// =============================================================================
// stat helpers
// =============================================================================

/*
 * Call stat(2) on `path` and extract all fields into the output pointers.
 * Returns 0 on success, -1 on failure (errno is set).
 */
int ignis_stat_call(
    const char *path,
    u64 *out_dev,
    u64 *out_ino,
    u32 *out_mode,
    u64 *out_nlink,
    u32 *out_uid,
    u32 *out_gid,
    i64 *out_size,
    i64 *out_atime,
    i64 *out_mtime,
    i64 *out_ctime,
    i64 *out_blksize,
    i64 *out_blocks
) {
    struct stat st;
    int rc = stat(path, &st);
    if (rc != 0) return -1;

    *out_dev     = (u64)st.st_dev;
    *out_ino     = (u64)st.st_ino;
    *out_mode    = (u32)st.st_mode;
    *out_nlink   = (u64)st.st_nlink;
    *out_uid     = (u32)st.st_uid;
    *out_gid     = (u32)st.st_gid;
    *out_size    = (i64)st.st_size;
    *out_atime   = (i64)st.st_atim.tv_sec;
    *out_mtime   = (i64)st.st_mtim.tv_sec;
    *out_ctime   = (i64)st.st_ctim.tv_sec;
    *out_blksize = (i64)st.st_blksize;
    *out_blocks  = (i64)st.st_blocks;
    return 0;
}

/*
 * Call fstat(2) on `fd` and extract all fields into the output pointers.
 * Returns 0 on success, -1 on failure (errno is set).
 */
int ignis_fstat_call(
    int fd,
    u64 *out_dev,
    u64 *out_ino,
    u32 *out_mode,
    u64 *out_nlink,
    u32 *out_uid,
    u32 *out_gid,
    i64 *out_size,
    i64 *out_atime,
    i64 *out_mtime,
    i64 *out_ctime,
    i64 *out_blksize,
    i64 *out_blocks
) {
    struct stat st;
    int rc = fstat(fd, &st);
    if (rc != 0) return -1;

    *out_dev     = (u64)st.st_dev;
    *out_ino     = (u64)st.st_ino;
    *out_mode    = (u32)st.st_mode;
    *out_nlink   = (u64)st.st_nlink;
    *out_uid     = (u32)st.st_uid;
    *out_gid     = (u32)st.st_gid;
    *out_size    = (i64)st.st_size;
    *out_atime   = (i64)st.st_atim.tv_sec;
    *out_mtime   = (i64)st.st_mtim.tv_sec;
    *out_ctime   = (i64)st.st_ctim.tv_sec;
    *out_blksize = (i64)st.st_blksize;
    *out_blocks  = (i64)st.st_blocks;
    return 0;
}

// =============================================================================
// dirent helpers
// =============================================================================

/*
 * Extract the name from a dirent pointer as a C string.
 * Returns NULL if entry is NULL.
 */
const char *ignis_dirent_name(void *entry) {
    if (entry == NULL) return NULL;
    return ((struct dirent *)entry)->d_name;
}

/*
 * Extract the inode number from a dirent pointer.
 */
u64 ignis_dirent_ino(void *entry) {
    if (entry == NULL) return 0;
    return (u64)((struct dirent *)entry)->d_ino;
}

/*
 * Extract the file type from a dirent pointer.
 * Returns d_type (DT_REG, DT_DIR, etc.) or 0 if unknown/NULL.
 */
u8 ignis_dirent_type(void *entry) {
    if (entry == NULL) return 0;
    return ((struct dirent *)entry)->d_type;
}

// =============================================================================
// open with mode (3-arg open)
// =============================================================================

/*
 * Three-argument open(2) wrapper for file creation with mode.
 */
int ignis_open3(const char *pathname, int flags, u32 mode) {
    return open(pathname, flags, (mode_t)mode);
}

#endif /* !_WIN32 */
