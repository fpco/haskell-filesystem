#ifndef HSSYSTEMFILEIO_SHIM_H
#define HSSYSTEMFILEIO_SHIM_H

#include <dirent.h>

struct dirent *
hssystemfileio_alloc_dirent();

void
hssystemfileio_free_dirent(struct dirent *);

int
hssystemfileio_readdir(DIR *dir, struct dirent *dirent);

char *
hssystemfileio_dirent_name(struct dirent *dirent);

int
hssystemfileio_mkdir(const char *path, int mode, int allow_exists);

#endif
