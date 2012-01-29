#include "hssystemfileio-win32.h"

#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>

int
hssystemfileio_copy_permissions(const wchar_t *old_path, const wchar_t *new_path)
{
	struct _stat st;
	int rc = _wstat(old_path, &st);
	if (rc == -1)
	{ return rc; }
	
	return _wchmod(new_path, st.st_mode);
}
