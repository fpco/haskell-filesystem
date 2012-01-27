#include "hssystemfileio.h"

#include <errno.h>
#include <stdlib.h>

struct dirent *
hssystemfileio_alloc_dirent()
{
	return malloc(sizeof (struct dirent));
}

void
hssystemfileio_free_dirent(struct dirent *p)
{
	free(p);
}

int
hssystemfileio_readdir(DIR *dir, struct dirent *dirent)
{
	struct dirent *dirent_result;
	while (1)
	{
		int rc = readdir_r(dir, dirent, &dirent_result);
		if (rc != 0)
		{
			if (errno == EINTR)
			{ continue; }
			
			return -1;
		}
		
		if (dirent_result == NULL)
		{ return 1; }
		
		return 0;
	}
}

char *
hssystemfileio_dirent_name(struct dirent *dirent)
{
	return dirent->d_name;
}
