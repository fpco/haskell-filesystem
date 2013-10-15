#ifndef HSSYSTEMFILEIO_UNIX_H
#define HSSYSTEMFILEIO_UNIX_H

struct dirent;

struct dirent *
hssystemfileio_alloc_dirent();

void
hssystemfileio_free_dirent(struct dirent *);

int
hssystemfileio_readdir(void *dir, struct dirent *dirent);

char *
hssystemfileio_dirent_name(struct dirent *dirent);

char *
hssystemfileio_getcwd(void);

int
hssystemfileio_isrealdir(const char *);

int
hssystemfileio_copy_permissions(const char *old_path, const char *new_path);

int
hssystemfileio_open_nonblocking(const char *path, int mode);

#endif
