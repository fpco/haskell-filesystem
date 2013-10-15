#ifndef HSSYSTEMFILEIO_WIN32_H
#define HSSYSTEMFILEIO_WIN32_H

#include <wchar.h>

int
hssystemfileio_copy_permissions(const wchar_t *old_path, const wchar_t *new_path);

#endif
