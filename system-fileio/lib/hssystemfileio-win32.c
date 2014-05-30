/*
Before including anything, we need to fix up MinGW's MSVCRT defines.

MinGW's <sys/stat.h> requires __MSVCRT_VERSION__ >= 0x0601 to define
_wstat64(). This is fine for the MinGW distributed with GHC, which sets
__MSVCRT_VERSION__ = 0x0700, but fails for the Haskell Platform because
its MinGW sets __MSVCRT_VERSION__ = 0x0600.

Therefore, we include <_mingw.h> first and bump its MSVCRT if necessary.
*/
#include <_mingw.h>

#if defined(__MSVCRT_VERSION__)
#  if __MSVCRT_VERSION__ < 0x0601
#    define __MSVCRT_VERSION__ 0x0601
#  endif
#else
#  define __MSVCRT_VERSION__ 0x0601
#endif

#include "hssystemfileio-win32.h"

#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>

int
hssystemfileio_copy_permissions(const wchar_t *old_path, const wchar_t *new_path)
{
	struct __stat64 st;
	int rc = _wstat64(old_path, &st);
	if (rc == -1)
	{ return rc; }
	
	return _wchmod(new_path, st.st_mode);
}
